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

describe("EditorInput", ({describe, _}) => {
  describe("flush", ({test, _}) => {
    test("simple sequence", ({expect}) => {
      let (bindings, _id) =
        EditorInput.empty
        |> EditorInput.addBinding(
             [Keycode(1, Modifiers.none), Keycode(2, Modifiers.none)],
             _ => true,
             "payloadAB",
           );

      let (bindings, _id) =
        bindings
        |> EditorInput.addBinding(
             [Keycode(1, Modifiers.none)],
             _ => true,
             "payloadA",
           );

      let (bindings, effects) = EditorInput.keyDown(aKeyNoModifiers, bindings);

      expect.equal(effects, []);

      let (bindings, effects) = EditorInput.flush(bindings);

      expect.equal(effects, [Execute("payloadA")]);
    })
  });
  describe("sequences", ({test, _}) => {
    test("simple sequence", ({expect}) => {
      let (bindings, _id) =
        EditorInput.empty
        |> EditorInput.addBinding(
             [Keycode(1, Modifiers.none), Keycode(2, Modifiers.none)],
             _ => true,
             "payload1",
           );

      let (bindings, effects) = EditorInput.keyDown(aKeyNoModifiers, bindings);

      expect.equal(effects, []);

      let (bindings, effects) = EditorInput.keyDown(bKeyNoModifiers, bindings);

      expect.equal(effects, [Execute("payload1")]);
    });
    test("same key in a sequence", ({expect}) => {
      let (bindings, _id) =
        EditorInput.empty
        |> EditorInput.addBinding(
             [Keycode(1, Modifiers.none), Keycode(1, Modifiers.none)],
             _ => true,
             "payloadAA",
           );

      let (bindings, effects) = EditorInput.keyDown(aKeyNoModifiers, bindings);

      expect.equal(effects, []);

      let (bindings, effects) = EditorInput.keyDown(aKeyNoModifiers, bindings);

      expect.equal(effects, [Execute("payloadAA")]);
    });
    test("partial match with another match", ({expect}) => {
      let (bindings, _id) =
        EditorInput.empty
        |> EditorInput.addBinding(
             [Keycode(1, Modifiers.none), Keycode(3, Modifiers.none)],
             _ => true,
             "payloadAC",
           );

      let (bindings, _id) =
        bindings
        |> EditorInput.addBinding(
             [Keycode(1, Modifiers.none)],
             _ => true,
             "payloadA",
           );

      let (bindings, effects) = EditorInput.keyDown(aKeyNoModifiers, bindings);

      expect.equal(effects, []);

      let (bindings, effects) = EditorInput.keyDown(aKeyNoModifiers, bindings);

      expect.equal(effects, [Execute("payloadA")]);

      let (_bindings, effects) = EditorInput.keyDown(cKeyNoModifiers, bindings);

      expect.equal(effects, [Execute("payloadAC")]);
    });
    test("partial match with unhandled", ({expect}) => {
      let (bindings, _id) =
        EditorInput.empty
        |> EditorInput.addBinding(
             [Keycode(1, Modifiers.none), Keycode(3, Modifiers.none)],
             _ => true,
             "payloadAC",
           );

      let (bindings, _id) =
        bindings
        |> EditorInput.addBinding(
             [Keycode(1, Modifiers.none)],
             _ => true,
             "payloadA",
           );

      let (bindings, effects) = EditorInput.keyDown(aKeyNoModifiers, bindings);

      expect.equal(effects, []);

      let (bindings, effects) = EditorInput.keyDown(bKeyNoModifiers, bindings);

      expect.equal(
        effects,
        [Unhandled(bKeyNoModifiers), Execute("payloadA")],
      );
    });
  });
  describe("key matching", ({test, _}) => {
    test("matches keycode", ({expect}) => {
      let (bindings, _id) =
        EditorInput.empty
        |> EditorInput.addBinding(
             [Keycode(1, Modifiers.none)],
             _ => true,
             "payload1",
           );

      let (_bindings, effects) = EditorInput.keyDown(aKeyNoModifiers, bindings);

      expect.equal(effects, [Execute("payload1")]);
    });
    test("key with no matches is unhandled", ({expect}) => {
      let bindings = EditorInput.empty;

      let (_bindings, effects) = EditorInput.keyDown(aKeyNoModifiers, bindings);

      expect.equal(effects, [Unhandled(aKeyNoModifiers)]);
    });
  });
  describe("remapping", ({test, _}) => {
    test("unhandled, single key remap", ({expect}) => {
      let (bindings, _id) =
        EditorInput.empty
        |> EditorInput.addMapping(
             [Keycode(1, Modifiers.none)],
             _ => true,
             [bKeyNoModifiers],
           );

      let (_bindings, effects) = EditorInput.keyDown(aKeyNoModifiers, bindings);

      expect.equal(effects, [Unhandled(bKeyNoModifiers)]);
    });

    test("unhandled, sequence remap", ({expect}) => {
      let (bindings, _id) =
        EditorInput.empty
        |> EditorInput.addMapping(
             [Keycode(1, Modifiers.none), Keycode(2, Modifiers.none)],
             _ => true,
             [cKeyNoModifiers],
           );

      let (bindings, effects) = EditorInput.keyDown(aKeyNoModifiers, bindings);
      expect.equal(effects, []);
      let (_bindings, effects) = EditorInput.keyDown(bKeyNoModifiers, bindings);

      expect.equal(effects, [Unhandled(cKeyNoModifiers)]);
    });
    /*test("unhandled, multiple keys", ({expect}) => {
        let (bindings, _id) =
          EditorInput.empty
          |> EditorInput.addMapping(
               [Keycode(1, Modifiers.none)],
               _ => true,
               [bKeyNoModifiers, cKeyNoModifiers],
             );

        let (_bindings, effects) = EditorInput.keyDown(aKeyNoModifiers, bindings);

        expect.equal(effects, [
          Unhandled(cKeyNoModifiers),
          Unhandled(bKeyNoModifiers)]);
      });*/
    test("with payload", ({expect}) => {
      let (bindings, _id) =
        EditorInput.empty
        |> EditorInput.addMapping(
             [Keycode(1, Modifiers.none)],
             _ => true,
             [bKeyNoModifiers],
           );
      let (bindings, _id) =
        bindings
        |> EditorInput.addBinding(
             [Keycode(2, Modifiers.none)],
             _ => true,
             "payload2",
           );

      let (_bindings, effects) = EditorInput.keyDown(aKeyNoModifiers, bindings);

      expect.equal(effects, [Execute("payload2")]);
    });
  });
});
