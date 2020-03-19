module Modifiers: {
  type t = {
    control: bool,
    alt: bool,
    shift: bool,
    meta: bool,
  };

  let create: (~control: bool, ~alt: bool, ~shift: bool, ~meta: bool) => t;

  let none: t;

  let equals: (t, t) => bool;
};

module Matcher: {
  type t =
    | Scancode(int, Modifiers.t)
    | Keycode(int, Modifiers.t);

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
  text: string,
};

module type Input = {
  type payload;
  type context;

  type t;

  let addBinding: (Matcher.sequence, context => bool, payload, t) => (t, int);
  let addMapping:
    (Matcher.sequence, context => bool, list(key), t) => (t, int);

  type effects =
    | Execute(payload)
    | Unhandled(key);

  let keyDown: (key, t) => (t, list(effects));
  let keyUp: (key, t) => (t, list(effects));
  let flush: t => (t, list(effects));

  let empty: t;
};

module Make:
  (Context: {
     type payload;
     type context;
   }) =>
   Input with type payload = Context.payload and type context = Context.context;
