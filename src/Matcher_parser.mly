%token <Matcher_internal.modifier> MODIFIER
%token <Key.t> BINDING
%token LT GT
%token EXCLAMATION
%token EOF

%start <Matcher_internal.t list> main

%%

main:
| phrase = list(expr) EOF { phrase }

expr:
| EXCLAMATION; LT e = keyup_binding GT { e }
| EXCLAMATION; s = keyup_binding { s }
| LT e = keydown_binding GT { e }
| s = keydown_binding { s }

keyup_binding:
| modifiers = list(MODIFIER); binding = BINDING { (Matcher_internal.Keyup, binding, modifiers) }

keydown_binding:
| modifiers = list(MODIFIER); binding = BINDING { (Matcher_internal.Keydown, binding, modifiers) }

