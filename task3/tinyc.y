%{
%}
%error_verbose

%union {
  int int_value;
  char* str_value;
}

%token <int_value> Integer
%token <str_value> Identifier

%%

program:
  external-declaration {}
  | program external-declaration {}
  ;
external-declaration:
  declaration {}
  | function-deﬁnition {}
  ;
declaration:
  'int' declarator-list {}
  ;
declarator-list:
  declarator {}
  | declarator-list ',' declarator {}
  ;
declarator:
  Identifier {}
  ;
function-deﬁnition:
  'int' declarator '(' parameter-type-listopt ')' compound-statement {}
  ;
parameter-type-list:
  parameter-declaration
  | parameter-type-list ',' parameter-declaration {}
  ;
parameter-declaration:
  'int' declarator {}
  ;
statement:
  ';' {}
  | expression ';' {}
  | compound-statement {}
  | 'if' '(' expression ')' statement {}
  | 'if' '(' expression ')' statement 'else' statement {}
  | 'while' '(' expression ')' statement {}
  | 'return' expression ';' {}
  ;
compound-statement:
  '{' declaration-listopt statement-listopt '}' {}
  ;
declaration-list:
  declaration {}
  | declaration-list declaration {}
  ;
statement-list:
  statement {}
  | statement-list statement {}
  ;
expression:
  assign-expr {}
  | expression ',' assign-expr {}
  ;
assign-expr:
  logical-OR-expr {}
  | Identifier '=' assign-expr {}
  ;
logical-OR-expr:
  logical-AND-expr {}
  | logical-OR-expr '||' logical-AND-expr {}
  ;
logical-AND-expr:
  equality-expr {}
  | logical-AND-expr '&&' equality-expr {}
  ;
equality-expr:
  relational-expr {}
  | equality-expr '==' relational-expr {}
  | equality-expr '!=' relational-expr {}
  ;
relational-expr:
  add-expr {}
  | relational-expr '<' add-expr {}
  | relational-expr '>' add-expr {}
  | relational-expr '<=' add-expr {}
  | relational-expr '>=' add-expr {}
  ;
add-expr:
  mult-expr {}
  | add-expr '+' mult-expr {}
  | add-expr '-' mult-expr {}
  ;
mult-expr:
  unary-expr {}
  | mult-expr '*' unary-expr {}
  | mult-expr '/' unary-expr {}
  ;
unary-expr:
  postﬁx-expr {}
  | '-' unary-expr {}
  ;
postﬁx-expr:
  primary-expr {}
  | Identifier '(' argument-expression-listopt ')' {}
  ;
primary-expr:
  Identifier {}
  | Integer {}
  | '(' expression ')' {}
  ;
argument-expression-list:
  assign-expr {}
  | argument-expression-list ',' assign-expr {}
  | declaration-list declaration {}
  ;

%%

extern int yylineno;

int yyerror(char *s) {
  fprintf(stderr, "%d: %s\n", yylineno, s);
  return 0;
}
int main() {
  yyparse();
}
