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

  type uniqueId;

  let addBinding: (Matcher.sequence, context => bool, payload, t) => (t, uniqueId);
  let addMapping:
    (Matcher.sequence, context => bool, list(key), t) => (t, uniqueId);

  type effects =
    | Execute(payload)
    | Text(string)
    | Unhandled(key);

  let keyDown: (~context: context, ~key: key, t) => (t, list(effects));
  let text: (~text: string, t) => (t, list(effects));
  let keyUp: (~context: context, ~key: key, t) => (t, list(effects));
  let flush: (~context: context, t) => (t, list(effects));

  let isPending: t => bool;

  let concat: (t, t) => t;

  let empty: t;
};

module UniqueId = {
  let nextId = ref(0); 

  let get = () => {
    let id = nextId^; 
    incr(nextId);
    id;
  };
};

module Make = (Config: {
                 type payload;
                 type context;
               }) => {
  type payload = Config.payload;
  type context = Config.context;

  type effects =
    | Execute(payload)
    | Text(string)
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

  type uniqueId = int;

  type gesture =
    | Down(key)
    | Up(key);

  type t = {
    bindings: list(binding),
    keys: list(gesture),
  };

  let concat = (first, second) => {
      bindings: first.bindings @ second.bindings,
      keys: [],
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

  let addBinding = (sequence, enabled, payload, keyBindings) => {
    let {bindings, _} = keyBindings;
    let id = UniqueId.get();
    let bindings = [
      {id, sequence, action: Dispatch(payload), enabled},
      ...bindings,
    ];

    let newBindings = {...keyBindings, bindings};
    (newBindings, id);
  };

  let addMapping = (sequence, enabled, keys, keyBindings) => {
    let {bindings, _} = keyBindings;
    let id = UniqueId.get();
    let bindings = [
      {id, sequence, action: Remap(keys), enabled},
      ...bindings,
    ];

    let newBindings = {...keyBindings, bindings};
    (newBindings, id);
  };

  let reset = (~keys=[], bindings) => {...bindings, keys};

  let getReadyBindings = bindings => {
    let filter = binding => binding.sequence == [];

    bindings |> List.filter(filter);
  };

  let flush = (~context, bindings) => {
    let allKeys = bindings.keys;

    let rec loop = (flush, revKeys, remainingKeys, effects) => {
      let candidateBindings =
        applyKeysToBindings(
          ~context,
          revKeys |> List.rev,
          bindings.bindings,
        );
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
            loop(
              flush,
              List.append(newKeys, revKeys),
              remainingKeys,
              effects,
            );
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
          loop(flush, otherKeys, [latestKey, ...remainingKeys], effects)
        }
      };
    };

    let (remainingKeys, effects) = loop(true, allKeys, [], []);

    let (remainingKeys, effects) = loop(false, remainingKeys, [], effects);

    let keys = remainingKeys;
    (reset(~keys, bindings), effects);
  };

  let handleKeyCore = (~context, gesture, bindings) => {
    let originalKeys = bindings.keys;
    let keys = [gesture, ...bindings.keys];

    let candidateBindings =
      applyKeysToBindings(~context, keys |> List.rev, bindings.bindings);

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

  let isPending = ({keys, _}) => keys != [];

  let keyDown = (~context, ~key, bindings) => {
    handleKeyCore(~context, Down(key), bindings);
  };

  let text = (~text, bindings) => (bindings, [])

  let keyUp = (~context, ~key, bindings) => {
    handleKeyCore(~context, Up(key), bindings);
  };

  let empty = {bindings: [], keys: []};
};
