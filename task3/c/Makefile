YACC=bison
YFLAGS= -d
CC=clang
LEX=flex
LFLAGS=
FILES= tinyc.tab.c lex.yy.c
all: parser
parser: $(FILES)
	gcc -o tinyc $(FILES)

lex.yy.o: lex.yy.cc
	$(CC) -c lex.yy.cc -o lex.yy.o

lex.yy.c: tinyc.l
	$(LEX) $(LFLAGS) -o lex.yy.c tinyc.l

tinyc.tab.c: tinyc.y
	$(YACC) $(YFLAGS) tinyc.y -o tinyc.tab.c
