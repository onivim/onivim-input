open TestFramework;
open EditorInput;

let aKeyNoModifiers: key = {
  scancode: 101,
  keycode: 1,
  modifiers: Modifiers.none,
  text: "a",
};

let bKeyNoModifiers: key = {
  scancode: 102,
  keycode: 2,
  modifiers: Modifiers.none,
  text: "b",
};

let cKeyNoModifiers: key = {
  scancode: 103,
  keycode: 3,
  modifiers: Modifiers.none,
  text: "c",
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
        Input.keyDown(~context=true, aKeyNoModifiers, bindings);

      expect.equal(effects, []);

      let (bindings, effects) = Input.flush(~context=true, bindings);

      expect.equal(effects, [Execute("payloadA")]);
    })
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

      let (bindings, effects) =
        Input.keyDown(~context=true, aKeyNoModifiers, bindings);

      expect.equal(effects, []);

      let (bindings, effects) =
        Input.keyDown(~context=true, bKeyNoModifiers, bindings);

      expect.equal(effects, [Execute("payload1")]);
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
        Input.keyDown(~context=true, aKeyNoModifiers, bindings);

      expect.equal(effects, []);

      let (bindings, effects) =
        Input.keyDown(~context=true, aKeyNoModifiers, bindings);

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
        Input.keyDown(~context=true, aKeyNoModifiers, bindings);

      expect.equal(effects, []);

      let (bindings, effects) =
        Input.keyUp(~context=true, aKeyNoModifiers, bindings);

      expect.equal(effects, []);

      let (bindings, effects) =
        Input.keyDown(~context=true, bKeyNoModifiers, bindings);

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
        Input.keyDown(~context=true, aKeyNoModifiers, bindings);

      expect.equal(effects, []);

      let (bindings, effects) =
        Input.keyUp(~context=true, aKeyNoModifiers, bindings);

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
        Input.keyDown(~context=true, aKeyNoModifiers, bindings);

      expect.equal(effects, []);

      let (bindings, effects) =
        Input.keyDown(~context=true, aKeyNoModifiers, bindings);

      expect.equal(effects, [Execute("payloadA")]);

      let (_bindings, effects) =
        Input.keyDown(~context=true, cKeyNoModifiers, bindings);

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
        Input.keyDown(~context=true, aKeyNoModifiers, bindings);

      expect.equal(effects, []);

      let (bindings, effects) =
        Input.keyDown(~context=true, bKeyNoModifiers, bindings);

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
        Input.keyDown(~context=false, aKeyNoModifiers, bindings);

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
        Input.keyDown(~context=false, aKeyNoModifiers, bindings);

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
        Input.keyDown(~context=true, aKeyNoModifiers, bindings);

      expect.equal(effects, [Execute("payload1")]);
    });
    test("key with no matches is unhandled", ({expect}) => {
      let bindings = Input.empty;

      let (_bindings, effects) =
        Input.keyDown(~context=true, aKeyNoModifiers, bindings);

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
        Input.keyDown(~context=true, aKeyNoModifiers, bindings);

      expect.equal(effects, [Unhandled(bKeyNoModifiers)]);
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
        Input.keyDown(~context=true, aKeyNoModifiers, bindings);
      expect.equal(effects, []);
      let (_bindings, effects) =
        Input.keyDown(~context=true, bKeyNoModifiers, bindings);

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
        Input.keyDown(~context=true, aKeyNoModifiers, bindings);

      expect.equal(effects, [Execute("payload2")]);
    });
  });
});
