open TestFramework;
open EditorInput;

let aKeyNoModifiers: key = {
  scancode: 101,
  keycode: 1,
  modifiers: Modifiers.none,
};

let bKeyNoModifiers: key = {
  scancode: 102,
  keycode: 2,
  modifiers: Modifiers.none,
};

let cKeyNoModifiers: key = {
  scancode: 103,
  keycode: 3,
  modifiers: Modifiers.none,
};

module Input =
  EditorInput.Make({
    type payload = string;
    type context = bool;
  });

describe("EditorInput", ({describe, _}) => {
  describe("flush", ({test, _}) => {
    test("simple sequence", ({expect}) => {
      let (bindings, _id) =
        Input.empty
        |> Input.addBinding(
             [
               Keydown(Keycode(1, Modifiers.none)),
               Keydown(Keycode(2, Modifiers.none)),
             ],
             _ => true,
             "payloadAB",
           );

      let (bindings, _id) =
        bindings
        |> Input.addBinding(
             [Keydown(Keycode(1, Modifiers.none))],
             _ => true,
             "payloadA",
           );

      let (bindings, effects) =
        Input.keyDown(~context=true, ~key=aKeyNoModifiers, bindings);

      expect.equal(effects, []);

      let (bindings, effects) = Input.flush(~context=true, bindings);

      expect.equal(effects, [Execute("payloadA")]);
    })
  });
  describe("text", ({test, _}) => {
    test("should immediately dispatch if no pending keys", ({expect}) => {
      let (bindings, _id) =
        Input.empty
        |> Input.addBinding(
             [Keydown(Keycode(1, Modifiers.none))],
             _ => true,
             "payloadA",
           );

      let (_bindings, effects) = Input.text(~text="a", bindings);
      expect.equal(effects, [Text("a")]);
    });
    test("should be held in sequence", ({expect}) => {
      let (bindings, _id) =
        Input.empty
        |> Input.addBinding(
             [
               Keydown(Keycode(1, Modifiers.none)),
               Keydown(Keycode(2, Modifiers.none)),
             ],
             _ => true,
             "payloadAB",
           );

      let (bindings, _effects) =
        Input.keyDown(~context=true, ~key=aKeyNoModifiers, bindings);

      let (bindings, _effects) = Input.text(~text="a", bindings);

      // Sequence fails - get text event
      let (_bindings, effects) =
        Input.keyDown(~context=true, ~key=cKeyNoModifiers, bindings);

      expect.equal(effects, [Text("a"), Unhandled(cKeyNoModifiers)]);
    });

    test(
      "text after successful binding should not be dispatched", ({expect}) => {
      let (bindings, _id) =
        Input.empty
        |> Input.addBinding(
             [
               Keydown(Keycode(1, Modifiers.none)),
               Keydown(Keycode(2, Modifiers.none)),
             ],
             _ => true,
             "payloadAB",
           );

      let (bindings, _effects) =
        Input.keyDown(~context=true, ~key=aKeyNoModifiers, bindings);

      let (bindings, _effects) = Input.text(~text="a", bindings);

      // Sequence fails - get text event
      let (bindings, effects) =
        Input.keyDown(~context=true, ~key=bKeyNoModifiers, bindings);

      expect.equal(effects, [Execute("payloadAB")]);

      let (_bindings, effects) = Input.text(~text="b", bindings);

      // Subsequent text input should be ignored
      expect.equal(effects, []);
    });

    test("text not dispatched in sequence", ({expect}) => {
      let (bindings, _id) =
        Input.empty
        |> Input.addBinding(
             [
               Keydown(Keycode(1, Modifiers.none)),
               Keydown(Keycode(2, Modifiers.none)),
             ],
             _ => true,
             "payloadAB",
           );

      let (bindings, _effects) =
        Input.keyDown(~context=true, ~key=aKeyNoModifiers, bindings);

      let (bindings, _effects) = Input.text(~text="a", bindings);

      // Sequence fails - get text event
      let (_bindings, effects) =
        Input.keyDown(~context=true, ~key=bKeyNoModifiers, bindings);

      expect.equal(effects, [Execute("payloadAB")]);
    });
  });
  describe("sequences", ({test, _}) => {
    test("simple sequence", ({expect}) => {
      let (bindings, _id) =
        Input.empty
        |> Input.addBinding(
             [
               Keydown(Keycode(1, Modifiers.none)),
               Keydown(Keycode(2, Modifiers.none)),
             ],
             _ => true,
             "payload1",
           );

      expect.equal(Input.isPending(bindings), false);

      let (bindings, effects) =
        Input.keyDown(~context=true, ~key=aKeyNoModifiers, bindings);

      expect.equal(Input.isPending(bindings), true);
      expect.equal(effects, []);

      let (bindings, effects) =
        Input.keyDown(~context=true, ~key=bKeyNoModifiers, bindings);

      expect.equal(effects, [Execute("payload1")]);
      expect.equal(Input.isPending(bindings), false);
    });
    test("same key in a sequence", ({expect}) => {
      let (bindings, _id) =
        Input.empty
        |> Input.addBinding(
             [
               Keydown(Keycode(1, Modifiers.none)),
               Keydown(Keycode(1, Modifiers.none)),
             ],
             _ => true,
             "payloadAA",
           );

      let (bindings, effects) =
        Input.keyDown(~context=true, ~key=aKeyNoModifiers, bindings);

      expect.equal(effects, []);

      let (bindings, effects) =
        Input.keyDown(~context=true, ~key=aKeyNoModifiers, bindings);

      expect.equal(effects, [Execute("payloadAA")]);
    });
    test("key up doesn't stop sequence", ({expect}) => {
      let (bindings, _id) =
        Input.empty
        |> Input.addBinding(
             [
               Keydown(Keycode(1, Modifiers.none)),
               Keydown(Keycode(2, Modifiers.none)),
             ],
             _ => true,
             "payloadAB",
           );

      let (bindings, effects) =
        Input.keyDown(~context=true, ~key=aKeyNoModifiers, bindings);

      expect.equal(effects, []);

      let (bindings, effects) =
        Input.keyUp(~context=true, ~key=aKeyNoModifiers, bindings);

      expect.equal(effects, []);

      let (bindings, effects) =
        Input.keyDown(~context=true, ~key=bKeyNoModifiers, bindings);

      expect.equal(effects, [Execute("payloadAB")]);
    });
    test("sequence with keyups", ({expect}) => {
      let (bindings, _id) =
        Input.empty
        |> Input.addBinding(
             [
               Keydown(Keycode(1, Modifiers.none)),
               Keyup(Keycode(1, Modifiers.none)),
             ],
             _ => true,
             "payloadA!A",
           );

      let (bindings, effects) =
        Input.keyDown(~context=true, ~key=aKeyNoModifiers, bindings);

      expect.equal(effects, []);

      let (bindings, effects) =
        Input.keyUp(~context=true, ~key=aKeyNoModifiers, bindings);

      expect.equal(effects, [Execute("payloadA!A")]);
    });
    test("partial match with another match", ({expect}) => {
      let (bindings, _id) =
        Input.empty
        |> Input.addBinding(
             [
               Keydown(Keycode(1, Modifiers.none)),
               Keydown(Keycode(3, Modifiers.none)),
             ],
             _ => true,
             "payloadAC",
           );

      let (bindings, _id) =
        bindings
        |> Input.addBinding(
             [Keydown(Keycode(1, Modifiers.none))],
             _ => true,
             "payloadA",
           );

      let (bindings, effects) =
        Input.keyDown(~context=true, ~key=aKeyNoModifiers, bindings);

      expect.equal(effects, []);

      let (bindings, effects) =
        Input.keyDown(~context=true, ~key=aKeyNoModifiers, bindings);

      expect.equal(effects, [Execute("payloadA")]);

      let (_bindings, effects) =
        Input.keyDown(~context=true, ~key=cKeyNoModifiers, bindings);

      expect.equal(effects, [Execute("payloadAC")]);
    });
    test("partial match with unhandled", ({expect}) => {
      let (bindings, _id) =
        Input.empty
        |> Input.addBinding(
             [
               Keydown(Keycode(1, Modifiers.none)),
               Keydown(Keycode(3, Modifiers.none)),
             ],
             _ => true,
             "payloadAC",
           );

      let (bindings, _id) =
        bindings
        |> Input.addBinding(
             [Keydown(Keycode(1, Modifiers.none))],
             _ => true,
             "payloadA",
           );

      let (bindings, effects) =
        Input.keyDown(~context=true, ~key=aKeyNoModifiers, bindings);

      expect.equal(effects, []);

      let (bindings, effects) =
        Input.keyDown(~context=true, ~key=bKeyNoModifiers, bindings);

      expect.equal(
        effects,
        [Unhandled(bKeyNoModifiers), Execute("payloadA")],
      );
    });
  });
  describe("enabled / disabled", ({test, _}) => {
    let identity = context => context;
    test("unhandled when enabled function returns false", ({expect}) => {
      let (bindings, _id) =
        Input.empty
        |> Input.addBinding(
             [Keydown(Keycode(1, Modifiers.none))],
             identity,
             "payload1",
           );

      let (_bindings, effects) =
        Input.keyDown(~context=false, ~key=aKeyNoModifiers, bindings);

      // Should be unhandled because the context function is [false]
      expect.equal(effects, [Unhandled(aKeyNoModifiers)]);
    });
    test("key sequence is unhandled when context is false", ({expect}) => {
      let (bindings, _id) =
        Input.empty
        |> Input.addBinding(
             [
               Keydown(Keycode(1, Modifiers.none)),
               Keydown(Keycode(3, Modifiers.none)),
             ],
             identity,
             "payloadAC",
           );

      let (bindings, effects) =
        Input.keyDown(~context=false, ~key=aKeyNoModifiers, bindings);

      expect.equal(effects, [Unhandled(aKeyNoModifiers)]);
    });
  });
  describe("key matching", ({test, _}) => {
    test("matches keycode", ({expect}) => {
      let (bindings, _id) =
        Input.empty
        |> Input.addBinding(
             [Keydown(Keycode(1, Modifiers.none))],
             _ => true,
             "payload1",
           );

      let (_bindings, effects) =
        Input.keyDown(~context=true, ~key=aKeyNoModifiers, bindings);

      expect.equal(effects, [Execute("payload1")]);
    });
    test("key with no matches is unhandled", ({expect}) => {
      let bindings = Input.empty;

      let (_bindings, effects) =
        Input.keyDown(~context=true, ~key=aKeyNoModifiers, bindings);

      expect.equal(effects, [Unhandled(aKeyNoModifiers)]);
    });
  });
  describe("remapping", ({test, _}) => {
    test("unhandled, single key remap", ({expect}) => {
      let (bindings, _id) =
        Input.empty
        |> Input.addMapping(
             [Keydown(Keycode(1, Modifiers.none))],
             _ => true,
             [bKeyNoModifiers],
           );

      let (_bindings, effects) =
        Input.keyDown(~context=true, ~key=aKeyNoModifiers, bindings);

      expect.equal(effects, [Unhandled(bKeyNoModifiers)]);
    });

    test("2-step recursive mapping", ({expect}) => {
      let (bindings, _id) =
        Input.empty
        |> Input.addMapping(
             [Keydown(Keycode(1, Modifiers.none))],
             _ => true,
             [bKeyNoModifiers],
           );

      let (bindings, _id) =
        bindings
        |> Input.addMapping(
             [Keydown(Keycode(2, Modifiers.none))],
             _ => true,
             [cKeyNoModifiers],
           );

      let (_bindings, effects) =
        Input.keyDown(~context=true, ~key=aKeyNoModifiers, bindings);

      expect.equal(effects, [Unhandled(cKeyNoModifiers)]);
    });

    test("recursive mapping doesn't hang", ({expect}) => {
      let (bindings, _id) =
        Input.empty
        |> Input.addMapping(
             [Keydown(Keycode(1, Modifiers.none))],
             _ => true,
             [aKeyNoModifiers],
           );

      let (_bindings, effects) =
        Input.keyDown(~context=true, ~key=aKeyNoModifiers, bindings);

      expect.equal(effects, [Unhandled(aKeyNoModifiers)]);
    });

    test("unhandled, sequence remap", ({expect}) => {
      let (bindings, _id) =
        Input.empty
        |> Input.addMapping(
             [
               Keydown(Keycode(1, Modifiers.none)),
               Keydown(Keycode(2, Modifiers.none)),
             ],
             _ => true,
             [cKeyNoModifiers],
           );

      let (bindings, effects) =
        Input.keyDown(~context=true, ~key=aKeyNoModifiers, bindings);
      expect.equal(effects, []);
      let (_bindings, effects) =
        Input.keyDown(~context=true, ~key=bKeyNoModifiers, bindings);

      expect.equal(effects, [Unhandled(cKeyNoModifiers)]);
    });
    /*test("unhandled, multiple keys", ({expect}) => {
        let (bindings, _id) =
          Input.empty
          |> Input.addMapping(
               [Keydown(Keycode(1, Modifiers.none)],
               _ => true,
               [bKeyNoModifiers, cKeyNoModifiers],
             );

        let (_bindings, effects) = Input.keyDown(context=true, aKeyNoModifiers, bindings);

        expect.equal(effects, [
          Unhandled(cKeyNoModifiers),
          Unhandled(bKeyNoModifiers)]);
      });*/
    test("with payload", ({expect}) => {
      let (bindings, _id) =
        Input.empty
        |> Input.addMapping(
             [Keydown(Keycode(1, Modifiers.none))],
             _ => true,
             [bKeyNoModifiers],
           );
      let (bindings, _id) =
        bindings
        |> Input.addBinding(
             [Keydown(Keycode(2, Modifiers.none))],
             _ => true,
             "payload2",
           );

      let (_bindings, effects) =
        Input.keyDown(~context=true, ~key=aKeyNoModifiers, bindings);

      expect.equal(effects, [Execute("payload2")]);
    });
  });
});
