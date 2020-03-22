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

  type t =
    | Keydown(keyMatcher)
    | Keyup(keyMatcher);

  type sequence = list(t);

  let parse:
    (
      ~getKeycode: string => option(int),
      ~getScancode: string => option(int),
      string
    ) =>
    result(sequence, string);
};

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

  /**
  [isPending(bindings)] returns true if there is a potential
  keybinding pending, false otherwise
  */
  let isPending: t => bool;

  let concat: (t, t) => t;

  let empty: t;
};

module Make:
  (Context: {
     type payload;
     type context;
   }) =>
   Input with type payload = Context.payload and type context = Context.context;
