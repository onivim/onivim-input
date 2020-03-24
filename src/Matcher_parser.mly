%token <Matcher_internal.modifier> MODIFIER
%token <Key.t> BINDING
%token ALLKEYSRELEASED
%token LT GT
%token EXCLAMATION
%token EOF

%start <Matcher_internal.t> main

%%

main:
| ALLKEYSRELEASED { Matcher_internal.AllKeysReleased }
| seq = list(sequence) EOF { Matcher_internal.Sequence(seq) }

sequence:
| key = keyup_bindings { key }
| key = keydown_bindings { key }

chord:
| modifiers = list(MODIFIER); binding = BINDING { (binding, modifiers) }

keyup_bindings:
| EXCLAMATION; LT e = keyup_binding GT { e }
| EXCLAMATION; s = keyup_binding { s }

keydown_bindings:
| LT e = keydown_binding GT { e }
| s = keydown_binding { s }

keyup_binding:
| modifiers = list(MODIFIER); binding = BINDING { (Matcher_internal.Keyup, binding, modifiers) }

keydown_binding:
| modifiers = list(MODIFIER); binding = BINDING { (Matcher_internal.Keydown, binding, modifiers) }

