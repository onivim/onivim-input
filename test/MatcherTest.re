open TestFramework;
open EditorInput;

let getKeycode =
  fun
  | Key.Character('a') => Some(1)
  | Key.Character('b') => Some(2)
  | Key.Character('0') => Some(50)
  | Key.Character('9') => Some(59)
  | Key.Tab => Some(98)
  | Key.Escape => Some(99)
  | Key.Up => Some(100)
  | Key.Left => Some(101)
  | Key.Right => Some(102)
  | Key.Down => Some(103)
  | Key.PageUp => Some(104)
  | Key.PageDown => Some(105)
  | Key.End => Some(106)
  | Key.Home => Some(107)
  | Key.Return => Some(108)
  | Key.Space => Some(109)
  | Key.Backspace => Some(110)
  | Key.Delete => Some(111)
  | Key.Pause => Some(112)
  | Key.CapsLock => Some(113)
  | Key.Insert => Some(114)
  | Key.Function(0) => Some(115)
  | Key.Function(19) => Some(134)
  | _ => None;

let getScancode =
  fun
  | _ => None;

let defaultParse = Matcher.parse(~getKeycode, ~getScancode);

let modifiersControl = {...Modifiers.none, control: true};

let modifiersShift = {...Modifiers.none, shift: true};

describe("Matcher", ({describe, _}) => {
  describe("parser", ({test, _}) => {
    test("all keys", ({expect}) => {
      open Matcher;
    // Exercise full set of keys described here:
    // https://code.visualstudio.com/docs/getstarted/keybindings#_accepted-keys
      let cases = [ 
        ("a", Keydown(Keycode(1, Modifiers.none))),
        ("A", Keydown(Keycode(1, Modifiers.none))),
        ("0", Keydown(Keycode(50, Modifiers.none))),
        ("9", Keydown(Keycode(59, Modifiers.none))),
        ("tab", Keydown(Keycode(98, Modifiers.none))),
        ("ESC", Keydown(Keycode(99, Modifiers.none))),
        ("up", Keydown(Keycode(100, Modifiers.none))),
        ("left", Keydown(Keycode(101, Modifiers.none))),
        ("right", Keydown(Keycode(102, Modifiers.none))),
        ("down", Keydown(Keycode(103, Modifiers.none))),
        ("PageUp", Keydown(Keycode(104, Modifiers.none))),
        ("pagedown", Keydown(Keycode(105, Modifiers.none))),
        ("end", Keydown(Keycode(106, Modifiers.none))),
        ("home", Keydown(Keycode(107, Modifiers.none))),
        ("enter", Keydown(Keycode(108, Modifiers.none))),
        ("cr", Keydown(Keycode(108, Modifiers.none))),
        ("escape", Keydown(Keycode(99, Modifiers.none))),
        ("space", Keydown(Keycode(109, Modifiers.none))),
        ("bs", Keydown(Keycode(110, Modifiers.none))),
        ("backspace", Keydown(Keycode(110, Modifiers.none))),
        ("del", Keydown(Keycode(111, Modifiers.none))),
        ("delete", Keydown(Keycode(111, Modifiers.none))),
        ("pause", Keydown(Keycode(112, Modifiers.none))),
        ("capslock", Keydown(Keycode(113, Modifiers.none))),
        ("insert", Keydown(Keycode(114, Modifiers.none))),
        ("f0", Keydown(Keycode(115, Modifiers.none))),
        ("f19", Keydown(Keycode(134, Modifiers.none))),
      ];

      let runCase = (case) => {
        let (keyString, matcher) = case;
        let result = defaultParse(keyString);

        expect.equal(result, Ok([matcher]));
      };

      let _: unit = cases
      |> List.iter(runCase);

    });
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
