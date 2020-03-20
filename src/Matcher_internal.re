type modifier =
  | Control
  | Shift
  | Alt
  | Meta;

type activation =
  | Keyup
  | Keydown;

type t = (activation, string, list(modifier));
