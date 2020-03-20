{
	open Matcher_parser
	open Matcher_internal
	
	exception Error

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

rule token = parse
| modifier as m
 { 
 	let m = String.lowercase_ascii m in
	try Hashtbl.find modifierTable m
	with Not_found ->
		BINDING(m)
 }
(* Single keys *)
| white { token lexbuf }
| ['a' - 'z' 'A' - 'Z' '0'-'9']  as i
 { BINDING (String.make 1 (Char.lowercase_ascii i)) }
| '<' { LT }
| '>' { GT }
| eof { EOF }
| _ { raise Error }
