%option noyywrap
%option yylineno
%{
#include "tinyc.tab.h"
int yyerror (char *s);
%}
%%

[0-9]+ {
  return INTEGER;
}
"("                         { return LEFT_PAR; }
")"                         { return RIGHT_PAR; }
"{"                         { return LEFT_BRACE; }
"}"                         { return RIGHT_BRACE; }
";"                         { return SEMICOLON; }
"int"                       { return TYPE_INT; }
"if"                        { return IF; }
"else"                      { return ELSE; }
"while"                     { return WHILE; }
"return"                    { return RETURN; }
"||"                        { return OP_LOGICAL_OR; }
"&&"                        { return OP_LOGICAL_AND; }
"=="                        { return OP_EQUAL; }
"!="                        { return OP_NOT_EQUAL; }
"<"                         { return OP_LT; }
">"                         { return OP_GT; }
"<="                        { return OP_LTE; }
">="                        { return OP_GTE; }
"+"                         { return OP_PLUS; }
"-"                         { return OP_MINUS; }
"*"                         { return OP_TIMES; }
"/"                         { return OP_DIVIDE; }
"="                         { return OP_ASSIGN; }
","                         { return COMMA; }
[A-Za-z][A-Za-z_0-9]* {
  return IDENTIFIER;
}
[ \t\n\r] ;
. yyerror("Error: invalid character");

%%
