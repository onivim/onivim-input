type t =
  | Scancode(int, Modifiers.t)
  | Keycode(int, Modifiers.t);

type sequence = list(t);

let parse = (~getKeycode, ~getScancode, str) => {
  let parse = lexbuf =>
    switch (Matcher_parser.main(Matcher_lexer.token, lexbuf)) {
    | exception Matcher_lexer.Error => Error("Error parsing binding: " ++ str)
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

  let finish = r => {
    let f = parseResult => {
      let (key, mods) = parseResult;
      switch (getKeycode(key)) {
      | None => Error("Unrecognized key: " ++ key)
      | Some(code) => Ok(Keycode(code, internalModsToMods(mods)))
      };
    };

    let bindings = r |> List.map(f);

    let errors = bindings |> List.filter(Result.is_error);

    if (List.length(errors) > 0) {
      let stringErrors =
        errors
        |> List.filter_map(
             fun
             | Error(msg) => Some(msg)
             | Ok(_) => None,
           );

      let firstError: string = List.nth(stringErrors, 0);
      Error(firstError);
    } else {
      bindings
      |> List.map(Result.to_option)
      |> List.filter_map(v => v)
      |> (out => Ok(out));
    };
  };

  str |> Lexing.from_string |> parse |> flatMap(finish);
};
