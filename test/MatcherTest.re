open TestFramework;
open EditorInput;

let getKeycode =
  fun
  | "a" => Some(1)
  | "b" => Some(2)
  | "Escape" => Some(99)
  | "Up" => Some(100)
  | _ => None;

let getScancode =
  fun
  | _ => None;

let defaultParse = Matcher.parse(~getKeycode, ~getScancode);

let modifiersControl = {...Modifiers.none, control: true};

let modifiersShift = {...Modifiers.none, shift: true};

describe("Matcher", ({describe, _}) => {
  describe("parser", ({test, _}) => {
    test("simple parsing", ({expect}) => {
      let result = defaultParse("a");
      expect.equal(result, Ok([Keydown(Keycode(1, Modifiers.none))]));

      let result = defaultParse("b");
      expect.equal(result, Ok([Keydown(Keycode(2, Modifiers.none))]));

      let result = defaultParse("c");
      expect.equal(Result.is_error(result), true);

      let result = defaultParse("esc");
      expect.equal(result, Ok([Keydown(Keycode(99, Modifiers.none))]));
    });
    test("vim bindings", ({expect}) => {
      let result = defaultParse("<a>");
      expect.equal(result, Ok([Keydown(Keycode(1, Modifiers.none))]));

      let result = defaultParse("<c-a>");
      expect.equal(result, Ok([Keydown(Keycode(1, modifiersControl))]));

      let result = defaultParse("<S-a>");
      expect.equal(result, Ok([Keydown(Keycode(1, modifiersShift))]));
    });
    test("vscode bindings", ({expect}) => {
      let result = defaultParse("Ctrl+a");
      expect.equal(result, Ok([Keydown(Keycode(1, modifiersControl))]));

      let result = defaultParse("ctrl+a");
      expect.equal(result, Ok([Keydown(Keycode(1, modifiersControl))]));
    });
    test("binding list", ({expect}) => {
      let result = defaultParse("ab");
      expect.equal(
        result,
        Ok([
          Keydown(Keycode(1, Modifiers.none)),
          Keydown(Keycode(2, Modifiers.none)),
        ]),
      );

      let result = defaultParse("a b");
      expect.equal(
        result,
        Ok([
          Keydown(Keycode(1, Modifiers.none)),
          Keydown(Keycode(2, Modifiers.none)),
        ]),
      );

      let result = defaultParse("<a>b");
      expect.equal(
        result,
        Ok([
          Keydown(Keycode(1, Modifiers.none)),
          Keydown(Keycode(2, Modifiers.none)),
        ]),
      );
      let result = defaultParse("<a><b>");
      expect.equal(
        result,
        Ok([
          Keydown(Keycode(1, Modifiers.none)),
          Keydown(Keycode(2, Modifiers.none)),
        ]),
      );

      let result = defaultParse("<c-a> Ctrl+b");
      expect.equal(
        result,
        Ok([
          Keydown(Keycode(1, modifiersControl)),
          Keydown(Keycode(2, modifiersControl)),
        ]),
      );
    });
    test("keyup", ({expect}) => {
      let result = defaultParse("!a");
      expect.equal(result, Ok([Keyup(Keycode(1, Modifiers.none))]));

      let result = defaultParse("a!a");
      expect.equal(
        result,
        Ok([
          Keydown(Keycode(1, Modifiers.none)),
          Keyup(Keycode(1, Modifiers.none)),
        ]),
      );

      let result = defaultParse("a !Ctrl+a");
      expect.equal(
        result,
        Ok([
          Keydown(Keycode(1, Modifiers.none)),
          Keyup(Keycode(1, modifiersControl)),
        ]),
      );

      let result = defaultParse("a !<C-A>");
      expect.equal(
        result,
        Ok([
          Keydown(Keycode(1, Modifiers.none)),
          Keyup(Keycode(1, modifiersControl)),
        ]),
      );
    });
  })
});
