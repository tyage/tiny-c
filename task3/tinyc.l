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
'+'|'*'|'-'|'/'|','|'('|')'|'{'|'}'|';'|'int'|'if'|'while'|'return'|'='|'||'|'&&'|
  '=='|'!='|'<'|'>'|'<='|'>=' {
  return *yytext;
}
[A-Za-z][A-Za-z_0-9]* {
  return Identifier;
}
[ \t\n] ;
. yyerror("Error: invalid character");

%%
