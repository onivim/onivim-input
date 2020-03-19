{
	open Matcher_parser
	open Matcher_internal
	
	exception Error
}

rule token = parse
| ['a' - 'z' 'A' - 'Z' '0'-'9'] as i
 { BINDING (String.make 1 i) }
| '<' { LT }
| '>' { GT }
| "c-" { VIM_MODIFIER (Control) }
| "C-" { VIM_MODIFIER (Control) }
| "s-" { VIM_MODIFIER (Shift) }
| "S-" { VIM_MODIFIER (Shift) }
| "a-" { VIM_MODIFIER (Alt) }
| "A-" { VIM_MODIFIER (Alt) }
| "D-" { VIM_MODIFIER (Meta) }
| "d-" { VIM_MODIFIER (Meta) }
| eof { EOF }
| _ { raise Error }
