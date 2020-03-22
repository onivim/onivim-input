type modifier =
  | Control
  | Shift
  | Alt
  | Meta;

type activation =
  | Keyup
  | Keydown;

type t = (activation, Key.t, list(modifier));
