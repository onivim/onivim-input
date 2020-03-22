{
	open Matcher_parser
	open Matcher_internal
	open Key
	
	exception Error

	exception UnrecognizedModifier of string

	let modifierTable = Hashtbl.create 64

	let _ = List.iter (fun (name, keyword) ->
	Hashtbl.add modifierTable name keyword) [
	"c-", MODIFIER (Control);
	"s-", MODIFIER (Shift);
	"a-", MODIFIER (Alt);
	"d-", MODIFIER (Meta);
	"ctrl+", MODIFIER (Control);
	"shift+", MODIFIER (Shift);
	"alt+", MODIFIER (Alt);
	"meta+", MODIFIER (Meta);
	"win+", MODIFIER (Meta);
	"cmd+", MODIFIER (Meta);
	]
}

let white = [' ' '\t']+

let alpha = ['a' - 'z' 'A' - 'Z']
let modifier = alpha+ ['-' '+']

let binding = ['a'-'z' 'A'-'Z' '0'-'9' '`' '-' '=' '[' ']' '\\' ';' '\'']

rule token = parse
| modifier as m
 { 
 	let m = String.lowercase_ascii m in
	try Hashtbl.find modifierTable m
	with Not_found ->
		raise (UnrecognizedModifier m)
 }
(* Single keys *)
| "esc" { BINDING (Escape) }
| "escape" { BINDING (Escape) }
| "up" { BINDING (Up) }
| "down" { BINDING (Down) }
| "left" { BINDING (Left) }
| "right" { BINDING (Right) }
| "tab" { BINDING (Tab) }
| "pageup" { BINDING (PageUp) }
| "pagedown" { BINDING (PageDown) }
| "cr" { BINDING (Return) }
| "enter" { BINDING (Return) }
| "space" { BINDING (Space) }
| "del" { BINDING (Delete) }
| "delete" { BINDING (Delete) }
| "pause" { BINDING (Pause) }
| "pausebreak" { BINDING (Pause) }
| "home" { BINDING (Home) }
| "end" { BINDING (End) }
| white { token lexbuf }
| '!' { EXCLAMATION }
| binding as i
 { BINDING (Character (i)) }
| '<' { LT }
| '>' { GT }
| eof { EOF }
| _ { raise Error }

and single_tokens = parse
| ['a' - 'z' 'A' - 'Z' '0'-'9']  as i
 { BINDING (Character (i)) }
