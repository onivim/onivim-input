%token <Matcher_internal.modifier> VIM_MODIFIER
%token <string> BINDING
%token LT GT
%token EOF

%start <Matcher_internal.t list> main

%%

main:
| phrase = list(expr) EOF { phrase }

expr:
| s = BINDING { (s, []) }
| LT e = vim_binding GT { e }

vim_binding:
| modifiers = list(VIM_MODIFIER); binding = BINDING { (binding, modifiers) };

