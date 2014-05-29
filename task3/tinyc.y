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
  external_declaration {}
  | program external_declaration {}
  ;
external_declaration:
  declaration {}
  | function_definition {}
  ;
declaration:
  'int' declarator_list {}
  ;
declarator_list:
  declarator {}
  | declarator_list ',' declarator {}
  ;
declarator:
  Identifier {}
  ;
function_definition:
  'int' declarator '(' parameter_type_listopt ')' compound_statement {}
  ;
parameter_type_list:
  parameter_declaration
  | parameter_type_list ',' parameter_declaration {}
  ;
parameter_declaration:
  'int' declarator {}
  ;
statement:
  ';' {}
  | expression ';' {}
  | compound_statement {}
  | 'if' '(' expression ')' statement {}
  | 'if' '(' expression ')' statement 'else' statement {}
  | 'while' '(' expression ')' statement {}
  | 'return' expression ';' {}
  ;
compound_statement:
  '{' declaration_listopt statement_listopt '}' {}
  ;
declaration_list:
  declaration {}
  | declaration_list declaration {}
  ;
statement_list:
  statement {}
  | statement_list statement {}
  ;
expression:
  assign_expr {}
  | expression ',' assign_expr {}
  ;
assign_expr:
  logical_OR_expr {}
  | Identifier '=' assign_expr {}
  ;
logical_OR_expr:
  logical_AND_expr {}
  | logical_OR_expr '||' logical_AND_expr {}
  ;
logical_AND_expr:
  equality_expr {}
  | logical_AND_expr '&&' equality_expr {}
  ;
equality_expr:
  relational_expr {}
  | equality_expr '==' relational_expr {}
  | equality_expr '!=' relational_expr {}
  ;
relational_expr:
  add_expr {}
  | relational_expr '<' add_expr {}
  | relational_expr '>' add_expr {}
  | relational_expr '<=' add_expr {}
  | relational_expr '>=' add_expr {}
  ;
add_expr:
  mult_expr {}
  | add_expr '+' mult_expr {}
  | add_expr '--' mult_expr {}
  ;
mult_expr:
  unary_expr {}
  | mult_expr '*' unary_expr {}
  | mult_expr '/' unary_expr {}
  ;
unary_expr:
  postfix_expr {}
  | '--' unary_expr {}
  ;
postfix_expr:
  primary_expr {}
  | Identifier '(' argument_expression_listopt ')' {}
  ;
primary_expr:
  Identifier {}
  | Integer {}
  | '(' expression ')' {}
  ;
argument_expression_list:
  assign_expr {}
  | argument_expression_list ',' assign_expr {}
  | declaration_list declaration {}
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
