%token <Matcher_internal.modifier> MODIFIER
%token <string> BINDING
%token LT GT
%token EOF

%start <Matcher_internal.t list> main

%%

main:
| phrase = list(expr) EOF { phrase }

expr:
| LT e = binding GT { e }
| s = binding { s }

binding:
| modifiers = list(MODIFIER); binding = BINDING { (binding, modifiers) };

