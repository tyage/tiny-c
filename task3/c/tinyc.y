%{
#include <stdio.h>
int yylex(void);
int yyerror (char *s);
%}
%error_verbose

%union {
  int int_value;
  char* str_value;
}

%token <int_value> INTEGER
%token <str_value> IDENTIFIER
%token <int_value> LEFT_PAR RIGHT_PAR LEFT_BRACE RIGHT_BRACE SEMICOLON
%token OP_ASSIGN COMMA
%token TYPE_INT
%token IF ELSE RETURN WHILE
%token <int_value> OP_LT OP_GT OP_LTE OP_GTE
%token <int_value> OP_LOGICAL_OR OP_LOGICAL_AND
%token <int_value> OP_EQUAL OP_NOT_EQUAL
%token <int_value> OP_PLUS OP_MINUS
%token <int_value> OP_TIMES OP_DIVIDE

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
  TYPE_INT declarator_list {}
  ;
declarator_list:
  declarator {}
  | declarator_list COMMA declarator {}
  ;
declarator:
  IDENTIFIER {}
  ;
function_definition:
  TYPE_INT declarator LEFT_PAR parameter_type_list RIGHT_PAR compound_statement {}
  TYPE_INT declarator LEFT_PAR RIGHT_PAR compound_statement {}
  ;
parameter_type_list:
  parameter_declaration
  | parameter_type_list COMMA parameter_declaration {}
  ;
parameter_declaration:
  TYPE_INT declarator {}
  ;
statement:
  SEMICOLON {}
  | expression SEMICOLON {}
  | compound_statement {}
  | IF LEFT_PAR expression RIGHT_PAR statement {}
  | IF LEFT_PAR expression RIGHT_PAR statement ELSE statement {}
  | WHILE LEFT_PAR expression RIGHT_PAR statement {}
  | RETURN expression SEMICOLON {}
  ;
compound_statement:
  LEFT_BRACE declaration_list statement_list RIGHT_BRACE {}
  LEFT_BRACE statement_list RIGHT_BRACE {}
  LEFT_BRACE declaration_list RIGHT_BRACE {}
  LEFT_BRACE RIGHT_BRACE {}
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
  | expression COMMA assign_expr {}
  ;
assign_expr:
  logical_OR_expr {}
  | IDENTIFIER OP_EQUAL assign_expr {}
  ;
logical_OR_expr:
  logical_AND_expr {}
  | logical_OR_expr OP_LOGICAL_OR logical_AND_expr {}
  ;
logical_AND_expr:
  equality_expr {}
  | logical_AND_expr OP_LOGICAL_AND equality_expr {}
  ;
equality_expr:
  relational_expr {}
  | equality_expr OP_EQUAL relational_expr {}
  | equality_expr OP_NOT_EQUAL relational_expr {}
  ;
relational_expr:
  add_expr {}
  | relational_expr OP_LT add_expr {}
  | relational_expr OP_GT add_expr {}
  | relational_expr OP_LTE add_expr {}
  | relational_expr OP_GTE add_expr {}
  ;
add_expr:
  mult_expr {}
  | add_expr OP_PLUS mult_expr {}
  | add_expr OP_MINUS mult_expr {}
  ;
mult_expr:
  unary_expr {}
  | mult_expr OP_TIMES unary_expr {}
  | mult_expr OP_DIVIDE unary_expr {}
  ;
unary_expr:
  postfix_expr {}
  | OP_MINUS unary_expr {}
  ;
postfix_expr:
  primary_expr {}
  | IDENTIFIER LEFT_PAR argument_expression_list RIGHT_PAR {}
  | IDENTIFIER LEFT_PAR RIGHT_PAR {}
  ;
primary_expr:
  IDENTIFIER {}
  | INTEGER {}
  | LEFT_PAR expression RIGHT_PAR {}
  ;
argument_expression_list:
  assign_expr {}
  | argument_expression_list COMMA assign_expr {}
  | declaration_list declaration {}
  ;

%%

extern int yylineno;

int yyerror(char *s) {
  fprintf(stderr, "%s\n", s);
  return 0;
}
int main() {
  yyparse();
}
