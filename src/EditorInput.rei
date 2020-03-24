module Key: {
  type t =
    | Character(char)
    | Function(int)
    | NumpadDigit(int)
    | Escape
    | Down
    | Up
    | Left
    | Right
    | Tab
    | PageUp
    | PageDown
    | Return
    | Space
    | Delete
    | Pause
    | Home
    | End
    | Backspace
    | CapsLock
    | Insert
    | NumpadMultiply
    | NumpadAdd
    | NumpadSeparator
    | NumpadSubtract
    | NumpadDecimal
    | NumpadDivide;

  let toString: t => string;
};

module Modifiers: {
  type t = {
    control: bool,
    alt: bool,
    altGr: bool,
    shift: bool,
    meta: bool,
  };

  let none: t;

  let equals: (t, t) => bool;
};

module Matcher: {
  type keyMatcher =
    | Scancode(int, Modifiers.t)
    | Keycode(int, Modifiers.t);

  type keyPress =
    | Keydown(keyMatcher)
    | Keyup(keyMatcher);

  type t =
    | Sequence(list(keyPress))
    | AllKeysReleased;

  let parse:
    (
      ~getKeycode: Key.t => option(int),
      ~getScancode: Key.t => option(int),
      string
    ) =>
    result(t, string);
};

type keyPress = {
  scancode: int,
  keycode: int,
  modifiers: Modifiers.t,
};

module type Input = {
  type command;
  type context;

  type t;

  type uniqueId;

  let addBinding: (Matcher.t, context => bool, command, t) => (t, uniqueId);

  let addMapping:
    (Matcher.t, context => bool, list(keyPress), t) => (t, uniqueId);

  type effects =
    | Execute(command)
    | Text(string)
    | Unhandled(keyPress);

  let keyDown: (~context: context, ~key: keyPress, t) => (t, list(effects));
  let text: (~text: string, t) => (t, list(effects));
  let keyUp: (~context: context, ~key: keyPress, t) => (t, list(effects));
  let flush: (~context: context, t) => (t, list(effects));

  /**
  [isPending(bindings)] returns true if there is a potential
  keybinding pending, false otherwise
  */
  let isPending: t => bool;

  let count: t => int;

  let concat: (t, t) => t;

  let empty: t;
};

module Make:
  (Context: {
     type command;
     type context;
   }) =>
   Input with type command = Context.command and type context = Context.context;
