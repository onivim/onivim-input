type modifier =
  | Control
  | Shift
  | Alt
  | Meta;

type activation =
  | Keyup
  | Keydown

type t =
| Key((activation, Key.t, list(modifier)))
| AllKeysReleased;
