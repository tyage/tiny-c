# 課題 3

以下の仕様を持つ Tiny C のパーサを作成せよ（アクションは空とする）．

else は C 言語同様，最も近い if 文に結び付くものとする（これについてはこの課題直後の説明を参照せよ）．

identiﬁer は英字，数字と下線記号 ‘_’ の列であり，最初の文字は英字とする．

大文字と小文字は区別すること．constant は数字の列である．

終端記号の区切りは任意個のスペース，タブまたは改行とする．

なお，この課題では lex ファイルにおいて yylval への代入は必要ない．

```
program:
  external-declaration
  program external-declaration
external-declaration:
  declaration
  function-deﬁnition
declaration:
  int declarator-list ;
declarator-list:
  declarator
  declarator-list , declarator
declarator:
  identiﬁer
function-deﬁnition:
  int declarator ( parameter-type-listopt ) compound-statement
parameter-type-list:
  parameter-declaration
  parameter-type-list , parameter-declaration
parameter-declaration:
  int declarator
statement:
  ;
  expression ;
  compound-statement
  if ( expression ) statement
  if ( expression ) statement else statement
  while ( expression ) statement
  return expression ;
compound-statement:
  { declaration-listopt statement-listopt }
declaration-list:
  declaration
  declaration-list declaration
statement-list:
  statement
  statement-list statement
expression:
  assign-expr
  expression , assign-expr
assign-expr:
  logical-OR-expr
  identiﬁer = assign-expr
logical-OR-expr:
  logical-AND-expr
  logical-OR-expr || logical-AND-expr
logical-AND-expr:
  equality-expr
  logical-AND-expr && equality-expr
equality-expr:
  relational-expr
  equality-expr == relational-expr
  equality-expr != relational-expr
relational-expr:
  add-expr
  relational-expr < add-expr
  relational-expr > add-expr
  relational-expr <= add-expr
  relational-expr >= add-expr
add-expr:
  mult-expr
  add-expr + mult-expr
  add-expr - mult-expr
mult-expr:
  unary-expr
  mult-expr * unary-expr
  mult-expr / unary-expr
unary-expr:
  postﬁx-expr
  - unary-expr
postﬁx-expr:
  primary-expr
  identiﬁer ( argument-expression-listopt )
primary-expr:
  identiﬁer
  constant
  ( expression )
argument-expression-list:
  assign-expr
  argument-expression-list , assign-expr
  declaration-list declaration
statement-list:
  statement
  statement-list statement
expression:
  assign-expr
  expression , assign-expr
assign-expr:
  logical-OR-expr
  identiﬁer = assign-expr
logical-OR-expr:
  logical-AND-expr
  logical-OR-expr || logical-AND-expr
logical-AND-expr:
  equality-expr
  logical-AND-expr && equality-expr
equality-expr:
  relational-expr
  equality-expr == relational-expr
  equality-expr != relational-expr
relational-expr:
  add-expr
  relational-expr < add-expr
  relational-expr > add-expr
  relational-expr <= add-expr
  relational-expr >= add-expr
add-expr:
  mult-expr
  add-expr + mult-expr
  add-expr - mult-expr
mult-expr:
  unary-expr
  mult-expr * unary-expr
  mult-expr / unary-expr
unary-expr:
  postﬁx-expr
  - unary-expr
postﬁx-expr:
  primary-expr
  identiﬁer ( argument-expression-listopt )
primary-expr:
  identiﬁer
  constant
  ( expression )
argument-expression-list:
  assign-expr
  argument-expression-list , assign-expr
```

## lex ファイルの雛形

```
%option noyywrap
%option yylineno
%{
#include "ﬁlename.tab.h"
%}
%%
· · ·
%%
```

## yacc ファイルの雛形

```
%{
%}
%error_verbose
%token Integer Identifer . . .
· · ·
%%
program:
· · ·
%%
extern int yylineno;
int yyerror(char *s) {
fprintf(stderr, "%d: %s\n", yylineno, s);
return 0;
}
main() {
yyparse();
}
```

文法定義に conﬂict が存在する場合，yacc は次のようなメッセージを表示する．

```
% bison -d calc.y
calc.y contains 1 shift/reduce conflict.
```

shift/reduce conflict は,あるトークンを読み込んだときにシフトと還元の両方が可能な場合に生じる.

re- duce/reduce conflict はあるトークンを読み込んだときに 2 通り以上の還元が可能な場合に生じる.

yacc は shift/reduce conflict が生じた場合,シフトすることを優先する.

例えば課題 3 では,‘else’ を読み込んだと きに if-else 文として ‘else’ をシフトするか,else 節を含まない if 文として還元するかの 2 通りのパースが 可能である.

C 言語の if 文と同じ意味にするのであれば else はシフトすればよいので,この if 文に関する conflict は無視して構わない.

一方,reduce/reduce conflict が生じた場合,yacc は適用可能なルールの中で最 初に現れるルールを選んで還元するが,これに頼ると可読性を損ない,また誤りを含み易いので reduce/reduce conflict は解消するのが望ましい.

具体的にどのように conflict が生じているかを調べるには,yacc (bison) の 実行時に -v というオプションを指定し,そのオプションによって生成された filename.output というファイルを調べればよい.
