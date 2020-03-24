type modifier =
  | Control
  | Shift
  | Alt
  | Meta;

type activation =
  | Keyup
  | Keydown;

type keyMatcher = (activation, Key.t, list(modifier));

type chordMatcher = (Key.t, list(modifier));

type t =
  | Chord(chordMatcher)
  | Sequence(list(keyMatcher))
  | AllKeysReleased;
