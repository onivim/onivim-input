open TestFramework;
open EditorInput;

let getKeycode =
  fun
  | "a" => Some(1)
  | "b" => Some(2)
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
      expect.equal(result, Ok([Keycode(1, Modifiers.none)]));

      let result = defaultParse("b");
      expect.equal(result, Ok([Keycode(2, Modifiers.none)]));

      let result = defaultParse("c");
      expect.equal(Result.is_error(result), true);
    });
    test("vim bindings", ({expect}) => {
      let result = defaultParse("<a>");
      expect.equal(result, Ok([Keycode(1, Modifiers.none)]));

      let result = defaultParse("<c-a>");
      expect.equal(result, Ok([Keycode(1, modifiersControl)]));

      let result = defaultParse("<S-a>");
      expect.equal(result, Ok([Keycode(1, modifiersShift)]));
    });
    test("vscode bindings", ({expect}) => {
      let result = defaultParse("Ctrl+a");
      expect.equal(result, Ok([Keycode(1, modifiersControl)]));

      let result = defaultParse("ctrl+a");
      expect.equal(result, Ok([Keycode(1, modifiersControl)]));
    });
    test("binding list", ({expect}) => {
      let result = defaultParse("ab");
      expect.equal(
        result,
        Ok([Keycode(1, Modifiers.none), Keycode(2, Modifiers.none)]),
      );

      let result = defaultParse("a b");
      expect.equal(
        result,
        Ok([Keycode(1, Modifiers.none), Keycode(2, Modifiers.none)]),
      );

      let result = defaultParse("<a>b");
      expect.equal(
        result,
        Ok([Keycode(1, Modifiers.none), Keycode(2, Modifiers.none)]),
      );
      let result = defaultParse("<a><b>");
      expect.equal(
        result,
        Ok([Keycode(1, Modifiers.none), Keycode(2, Modifiers.none)]),
      );

      let result = defaultParse("<c-a> Ctrl+b");
      expect.equal(
        result,
        Ok([Keycode(1, modifiersControl), Keycode(2, modifiersControl)]),
      );
    });
  })
});
