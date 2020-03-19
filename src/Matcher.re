type t =
  | Scancode(int, Modifiers.t)
  | Keycode(int, Modifiers.t);

type sequence = list(t);

let parse = (~getKeycode: string => int, ~getScancode: string => int, str) => {
  Error("Not implemented");
};
