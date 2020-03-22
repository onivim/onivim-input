type t = 
| Character(char)
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
| Pause;

let to_string = fun
| Character(c) => Printf.sprintf("Character(%c)", c)
| Escape => "Escape"
| Down => "Down"
| Up => "Up"
| Left => "Left"
| Right => "Right"
| Tab => "Tab"
| PageUp => "PageUp"
| PageDown => "PageDown"
| Return => "Return"
| Space => "Space"
| Delete => "Delete"
| Pause => "PauseBreak";
