module Modifiers = Modifiers;
module Matcher = Matcher;

type key = {
  scancode: int,
  keycode: int,
  modifiers: Modifiers.t,
  text: string,
};

module type Input = {
  type payload;
  type context;

  type t;

  let addBinding: (Matcher.sequence, context => bool, payload, t) => (t, int);
  let addMapping:
    (Matcher.sequence, context => bool, list(key), t) => (t, int);

  type effects =
    | Execute(payload)
    | Unhandled(key);

  let keyDown: (~context: context, key, t) => (t, list(effects));
  let keyUp: (~context: context, key, t) => (t, list(effects));
  let flush: (~context: context, t) => (t, list(effects));

  let empty: t;
};

module Make = (Config: {
                 type payload;
                 type context;
               }) => {
  type payload = Config.payload;
  type context = Config.context;

  type effects =
    | Execute(payload)
    | Unhandled(key);

  type action =
    | Dispatch(payload)
    | Remap(list(key));

  type binding = {
    id: int,
    sequence: Matcher.sequence,
    action,
    enabled: context => bool,
  };

  type gesture =
    | Down(key)
    | Up(key);

  type t = {
    nextId: int,
    allBindings: list(binding),
    keys: list(gesture),
  };

  let keyMatches = (keyMatcher, key: gesture) => {
    Matcher.(
      {
        switch (keyMatcher, key) {
        | (Keydown(Scancode(scancode, mods)), Down(key)) =>
          key.scancode == scancode && Modifiers.equals(mods, key.modifiers)
        | (Keydown(Keycode(keycode, mods)), Down(key)) =>
          key.keycode == keycode && Modifiers.equals(mods, key.modifiers)
        | (Keyup(Scancode(scancode, mods)), Up(key)) =>
          key.scancode == scancode && Modifiers.equals(mods, key.modifiers)
        | (Keyup(Keycode(keycode, mods)), Up(key)) =>
          key.keycode == keycode && Modifiers.equals(mods, key.modifiers)
        | _ => false
        };
      }
    );
  };

  let applyKeyToBinding = (~context, key, binding) =>
    if (!binding.enabled(context)) {
      None;
    } else {
      switch (binding.sequence) {
      | [hd, ...tail] when keyMatches(hd, key) =>
        Some({...binding, sequence: tail})
      | [] => None
      | _ => None
      };
    };

  let applyKeyToBindings = (~context, key, bindings) => {
    List.filter_map(applyKeyToBinding(~context, key), bindings);
  };

  let applyKeysToBindings = (~context, keys, bindings) => {
    let bindingsWithKeyUp =
      keys
      |> List.fold_left(
           (acc, curr) => {applyKeyToBindings(~context, curr, acc)},
           bindings,
         );

    let consumedBindings =
      bindingsWithKeyUp
      |> List.fold_left(
           (acc, curr) => {
             Hashtbl.add(acc, curr.id, true);
             acc;
           },
           Hashtbl.create(16),
         );

    let unusedBindings =
      bindings
      |> List.filter(binding =>
           Stdlib.Option.is_none(
             Hashtbl.find_opt(consumedBindings, binding.id),
           )
         );

    let onlyDownKeys =
      keys
      |> List.filter_map(
           fun
           | Down(key) => Some(Down(key))
           | Up(key) => None,
         );

    let bindingsWithJustKeyDown =
      onlyDownKeys
      |> List.fold_left(
           (acc, curr) => {applyKeyToBindings(~context, curr, acc)},
           unusedBindings,
         );

    bindingsWithKeyUp @ bindingsWithJustKeyDown;
  };

  let addBinding = (sequence, enabled, payload, bindings) => {
    let {nextId, allBindings, _} = bindings;
    let allBindings = [
      {id: nextId, sequence, action: Dispatch(payload), enabled},
      ...allBindings,
    ];

    let newBindings = {...bindings, allBindings, nextId: nextId + 1};
    (newBindings, nextId);
  };

  let addMapping = (sequence, enabled, keys, bindings) => {
    let {nextId, allBindings, _} = bindings;
    let allBindings = [
      {id: nextId, sequence, action: Remap(keys), enabled},
      ...allBindings,
    ];

    let newBindings = {...bindings, allBindings, nextId: nextId + 1};
    (newBindings, nextId);
  };

  let reset = (~keys=[], bindings) => {...bindings, keys};

  let getReadyBindings = bindings => {
    let filter = binding => binding.sequence == [];

    bindings |> List.filter(filter);
  };

  let isRemap = ({action, _}) =>
    switch (action) {
    | Dispatch(_) => false
    | Remap(_) => true
    };

  module Constants = {
    let maxRecursiveDepth = 10;
  };

  let flush = (~context, bindings) => {
    let allKeys = bindings.keys;

    let rec loop = (flush, revKeys, remainingKeys, effects, iterationCount) => {
      let candidateBindings =
        bindings.allBindings
        |> applyKeysToBindings(~context, revKeys |> List.rev);

      // If we've hit the recursion limit for remaps... filter out remaps
      let candidateBindings =
        if (iterationCount > Constants.maxRecursiveDepth) {
          candidateBindings |> List.filter(binding => !isRemap(binding));
        } else {
          candidateBindings;
        };

      let readyBindings = getReadyBindings(candidateBindings);
      let readyBindingCount = List.length(readyBindings);
      let candidateBindingCount = List.length(candidateBindings);

      let potentialBindingCount = candidateBindingCount - readyBindingCount;

      switch (List.nth_opt(readyBindings, 0)) {
      | Some(binding) =>
        if (flush || potentialBindingCount == 0) {
          switch (binding.action) {
          | Dispatch(payload) => (
              remainingKeys,
              [Execute(payload), ...effects],
            )
          | Remap(keys) =>
            let newKeys = keys |> List.map(k => Down(k)) |> List.rev;
            loop(flush, newKeys, remainingKeys, effects, iterationCount + 1);
          };
        } else {
          (List.append(revKeys, remainingKeys), effects);
        }
      // Queue keys -
      | None when potentialBindingCount > 0 => (
          // We have more bindings available, so just stash our keys and quit
          List.append(revKeys, remainingKeys),
          effects,
        )
      // No candidate bindings... try removing a key and processing bindings
      | None =>
        switch (revKeys) {
        | [] =>
          // No keys left, we're done here
          (remainingKeys, effects)
        | [Down(latestKey)] =>
          // At the last key... if we got here, we couldn't find any match for this key
          ([], [Unhandled(latestKey), ...effects])
        | [Up(latestKey)] =>
          // At the last key... if we got here, we couldn't find any match for this key
          ([], effects)
        | [latestKey, ...otherKeys] =>
          // Try a subset of keys
          loop(
            flush,
            otherKeys,
            [latestKey, ...remainingKeys],
            effects,
            iterationCount,
          )
        }
      };
    };

    let (remainingKeys, effects) = loop(true, allKeys, [], [], 0);

    let (remainingKeys, effects) =
      loop(false, remainingKeys, [], effects, 0);

    let keys = remainingKeys;
    (reset(~keys, bindings), effects);
  };

  let handleKeyCore = (~context, gesture, bindings) => {
    let originalKeys = bindings.keys;
    let keys = [gesture, ...bindings.keys];

    let candidateBindings =
      applyKeysToBindings(~context, keys |> List.rev, bindings.allBindings);

    let readyBindings = getReadyBindings(candidateBindings);
    let readyBindingCount = List.length(readyBindings);
    let candidateBindingCount = List.length(candidateBindings);

    let potentialBindingCount = candidateBindingCount - readyBindingCount;

    if (potentialBindingCount > 0) {
      ({...bindings, keys}, []);
    } else {
      switch (List.nth_opt(readyBindings, 0)) {
      | Some(binding) =>
        switch (binding.action) {
        | Dispatch(payload) => (reset(bindings), [Execute(payload)])
        | Remap(remappedKeys) =>
          let keys =
            List.append(originalKeys, List.map(k => Down(k), remappedKeys));
          flush(~context, {...bindings, keys});
        }
      | None => flush(~context, {...bindings, keys})
      };
    };
  };

  let keyDown = (~context, key, bindings) => {
    handleKeyCore(~context, Down(key), bindings);
  };

  let keyUp = (~context, key, bindings) => {
    handleKeyCore(~context, Up(key), bindings);
  };

  let empty = {nextId: 0, allBindings: [], keys: []};
};
