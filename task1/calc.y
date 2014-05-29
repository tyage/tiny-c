%{
#include <stdio.h>
int yylex(void);
int yyerror (char *s);
%}
%token Integer
%%
program:
expr { printf("%d\n", $1); }
;
expr:
mult_expr { $$ = $1; }
| mult_expr '+' expr { $$ = $1 + $3; }
;
mult_expr:
Integer { $$ = $1; }
| Integer '*' mult_expr { $$ = $1 * $3; }
;
%%
int yyerror(char *s) {
fprintf(stderr, "%s\n", s);
return 0;
}
int main() {
yyparse();
}
