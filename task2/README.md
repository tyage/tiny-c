# 課題 2

課題 1 の yacc ファイルの expr に引き算を追加せよ．ここで

```
expr:
mult_expr { $$ = $1; }
| mult_expr ’+’ expr { $$ = $1 + $3; }
| mult_expr ’-’ expr { $$ = $1 - $3; }
8;
```

とすると，結果は普通の引き算の意味とは異なるものとなる．  
例として入力 1-2-3 を与えて確認せよ．  
また，その理由を考察せよ．

さらに，普通の引き算の意味を反映するように修正せよ．

[ヒント] 普通，引き算は「左結合」であると考えられる．  
つまり，上の入力例は (1-2)-3 と解釈されるのが普通である．  
これに対し，上の生成規則のような「右再帰」で定義される結合子は「右結合」として定義される．