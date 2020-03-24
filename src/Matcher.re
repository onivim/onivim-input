type keyMatcher =
  | Scancode(int, Modifiers.t)
  | Keycode(int, Modifiers.t);

type keyPress =
  | Keydown(keyMatcher)
  | Keyup(keyMatcher);

type t =
  | Chord(list(keyMatcher))
  | Sequence(list(keyPress))
  | AllKeysReleased;

type sequence = list(t);

let parse = (~getKeycode, ~getScancode, str) => {
  let parse = lexbuf =>
    switch (Matcher_parser.main(Matcher_lexer.token, lexbuf)) {
    | exception Matcher_lexer.Error => Error("Error parsing binding: " ++ str)
    | exception (Matcher_lexer.UnrecognizedModifier(m)) =>
      Error("Unrecognized modifier:" ++ m ++ " in: " ++ str)
    | exception Matcher_parser.Error =>
      Error("Error parsing binding: " ++ str)
    | v => Ok(v)
    };

  let flatMap = (f, r) => Result.bind(r, f);

  let internalModsToMods = modList => {
    open Matcher_internal;
    let rec loop = (mods, modList) =>
      switch (modList) {
      | [] => mods
      | [Control, ...tail] => loop(Modifiers.{...mods, control: true}, tail)
      | [Shift, ...tail] => loop(Modifiers.{...mods, shift: true}, tail)
      | [Alt, ...tail] => loop(Modifiers.{...mods, alt: true}, tail)
      | [Meta, ...tail] => loop(Modifiers.{...mods, meta: true}, tail)
      };

    loop(Modifiers.none, modList);
  };

  module Internal = {
    let sequence: list(result('a, 'b)) => result(list('a), 'b) =
      (items: list(result('a, 'b))) => {
        let rec loop = (items, acc) => {
          switch (items) {
          | [Ok(hd), ...tail] => loop(tail, [hd, ...acc])
          | [Error(msg), ..._] => Error(msg)
          | [] => Ok(acc |> List.rev)
          };
        };

        loop(items, []);
      };
  };

  let finish = r => {
    switch (r) {
    | Matcher_internal.Chord(keys) =>
      let resolveChordKey = ((key, mods)) => {
        switch (getKeycode(key)) {
        | None => Error("Unrecognized key: " ++ Key.toString(key))
        | Some(code) => Ok(Keycode(code, internalModsToMods(mods)))
        };
      };
      keys
      |> List.map(resolveChordKey)
      |> Internal.sequence
      |> Result.map(out => Chord(out));
    | Matcher_internal.AllKeysReleased => Ok(AllKeysReleased)
    | Matcher_internal.Sequence(keys) =>
      let resolveSequenceKey:
        Matcher_internal.keyMatcher => result(keyPress, string) = (
        ((activation, key, mods)) => {
          switch (getKeycode(key)) {
          | None => Error("Unrecognized key: " ++ Key.toString(key))
          | Some(code) =>
            switch (activation) {
            | Matcher_internal.Keydown =>
              Ok(Keydown(Keycode(code, internalModsToMods(mods))))
            | Matcher_internal.Keyup =>
              Ok(Keyup(Keycode(code, internalModsToMods(mods))))
            }
          };
        }
      );
      let bindings: list(result(keyPress, string)) =
        keys |> List.map(resolveSequenceKey);

      let out: result(list(keyPress), string) =
        bindings |> Internal.sequence;

      out |> Result.map(out => Sequence(out));
    };
  };

  str
  |> String.lowercase_ascii
  |> Lexing.from_string
  |> parse
  |> flatMap(finish);
};
