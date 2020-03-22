module Modifiers = Modifiers;
module Matcher = Matcher;

type key = {
  scancode: int,
  keycode: int,
  modifiers: Modifiers.t,
};

module type Input = {
  type payload;
  type context;

  type t;

  type uniqueId;

  let addBinding:
    (Matcher.sequence, context => bool, payload, t) => (t, uniqueId);
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

module KeyDownId = {
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

  type keyDownId = int;

  type gesture =
    | Down(keyDownId, key)
    | Up(key);

  type textEntry = {
    keyDownId,
    text: string,
  };

  type t = {
    lastDownKey: option(keyDownId),
    suppressText: bool,
    bindings: list(binding),
    text: list(textEntry),
    keys: list(gesture),
  };

  let concat = (first, second) => {
    suppressText: false,
    lastDownKey: None,
    bindings: first.bindings @ second.bindings,
    keys: [],
    text: [],
  };

  let keyMatches = (keyMatcher, key: gesture) => {
    Matcher.(
      {
        switch (keyMatcher, key) {
        | (Keydown(Scancode(scancode, mods)), Down(_id, key)) =>
          key.scancode == scancode && Modifiers.equals(mods, key.modifiers)
        | (Keydown(Keycode(keycode, mods)), Down(_id, key)) =>
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
           | Down(id, key) => Some(Down(id, key))
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

  let reset = (~keys=[], ~text=[], bindings) => {
    ...bindings,
    lastDownKey: None,
    text,
    keys,
  };

  let getReadyBindings = bindings => {
    let filter = binding => binding.sequence == [];

    bindings |> List.filter(filter);
  };

  let keyIdsToHashtable = (keys: list(gesture)) => {
    let ret = Hashtbl.create(64);

    keys
    |> List.iter(curr => {
         switch (curr) {
         | Up(_key) => ()
         | Down(id, _key) => Hashtbl.add(ret, id, true)
         }
       });

    ret;
  };

  let getTextMatchingKeys = (text, keys) => {
    let hash = keyIdsToHashtable(keys);

    text |> List.filter(textEntry => Hashtbl.mem(hash, textEntry.keyDownId));
  };

  let getTextNotMatchingKeys = (text, keys) => {
    let hash = keyIdsToHashtable(keys);

    text |> List.filter(textEntry => !Hashtbl.mem(hash, textEntry.keyDownId));
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

    let rec loop =
            (
              ~flush,
              revKeys,
              remainingText: list(textEntry),
              remainingKeys,
              effects,
              iterationCount,
            ) => {
      let candidateBindings =
        applyKeysToBindings(~context, revKeys |> List.rev, bindings.bindings);

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
          // Filter out any 'text' entries that are associated with the keys for this finding
          let remainingText = getTextNotMatchingKeys(remainingText, revKeys);

          switch (binding.action) {
          | Dispatch(payload) => (
              remainingKeys,
              remainingText,
              [Execute(payload), ...effects],
            )
          | Remap(keys) =>
            let newKeys =
              keys |> List.map(k => Down(KeyDownId.get(), k)) |> List.rev;
            loop(
              ~flush,
              newKeys,
              remainingText,
              remainingKeys,
              effects,
              iterationCount + 1,
            );
          };
        } else {
          (List.append(revKeys, remainingKeys), remainingText, effects);
        }
      // Queue keys -
      | None when potentialBindingCount > 0 => (
          // We have more bindings available, so just stash our keys and quit
          List.append(revKeys, remainingKeys),
          remainingText,
          effects,
        )
      // No candidate bindings... try removing a key and processing bindings
      | None =>
        switch (revKeys) {
        | [] =>
          // No keys left, we're done here
          (remainingKeys, remainingText, effects)
        | [Down(keyDownId, latestKey)] =>
          let textEffects =
            remainingText
            |> List.filter(textEntry => textEntry.keyDownId == keyDownId)
            |> List.map((textEntry: textEntry) => Text(textEntry.text));

          let remainingText =
            remainingText
            |> List.filter(textEntry => textEntry.keyDownId != keyDownId);

          // At the last key... if we got here, we couldn't find any match for this key
          (
            [],
            remainingText,
            [Unhandled(latestKey)] @ textEffects @ effects,
          );
        | [Up(latestKey)] =>
          // At the last key... if we got here, we couldn't find any match for this key
          ([], remainingText, effects)
        | [latestKey, ...otherKeys] =>
          // Try a subset of keys
          loop(
            ~flush,
            otherKeys,
            remainingText,
            [latestKey, ...remainingKeys],
            effects,
            iterationCount,
          )
        }
      };
    };

    let (remainingKeys, remainingText, effects) =
      loop(~flush=true, allKeys, bindings.text, [], [], 0);

    let (remainingKeys, remainingText, effects) =
      loop(~flush=false, remainingKeys, remainingText, [], effects, 0);

    let keys = remainingKeys;

    // The text used for the commands was filtered out, so any now-unmatched
    // text is unhandled
    let unhandledText = getTextNotMatchingKeys(remainingText, keys);
    let currentText = getTextMatchingKeys(remainingText, keys);

    let textEffects =
      unhandledText
      |> List.map((textEntry: textEntry) => Text(textEntry.text));

    let text = currentText;
    (reset(~keys, ~text, bindings), textEffects @ effects);
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
        let text = getTextNotMatchingKeys(bindings.text, keys);
        switch (binding.action) {
        | Dispatch(payload) => (
            reset({...bindings, suppressText: true, text}),
            [Execute(payload)],
          )
        | Remap(remappedKeys) =>
          let keys =
            List.append(
              originalKeys,
              List.map(k => Down(KeyDownId.get(), k), remappedKeys),
            );
          flush(~context, {...bindings, suppressText: true, text, keys});
        };
      | None => flush(~context, {...bindings, keys})
      };
    };
  };

  let isPending = ({keys, _}) => keys != [];

  let keyDown = (~context, ~key, bindings) => {
    let id = KeyDownId.get();
    handleKeyCore(
      ~context,
      Down(id, key),
      {...bindings, lastDownKey: Some(id)},
    );
  };

  let text = (~text, bindings) =>
    // The last key down participating in binding,
    // so we'll ignore text until we get a keyup
    if (bindings.suppressText) {
      (bindings, []);
    } else {
      switch (bindings.lastDownKey) {
      // If there is a pending key, hold on to the text input
      // until the gesture is completed
      | Some(keyDownId) => (
          {...bindings, text: [{keyDownId, text}, ...bindings.text]},
          [],
        )
      // Otherwise, just dispatch the Text event
      | None => (bindings, [Text(text)])
      };
    };

  let keyUp = (~context, ~key, bindings) => {
    handleKeyCore(~context, Up(key), {...bindings, suppressText: false});
  };

  let empty = {
    suppressText: false,
    text: [],
    lastDownKey: None,
    bindings: [],
    keys: [],
  };
};
