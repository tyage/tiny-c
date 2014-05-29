# 課題 1

calc.l と calc.y を作成し，実際にコンパイル，実行してみよ．

```sh
% bison -d calc.y
% flex calc.l
% gcc -o calc calc.tab.c lex.yy.c
% cat sample
1+2*3+4
% ./calc < sample
11```