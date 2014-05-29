%option noyywrap
%option yylineno
%{
#include "tinyc.tab.h"
%}

%%

{digit}+ {
  yylval = atoi(yytext);
  return Integer;
}
"+"|"*"|"-" {
  return *yytext;
}
[ \t\n] ; /* スペース，タブ，改行は無視 */
. yyerror("Error: invalid character");

%%
