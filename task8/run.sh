#!/bin/bash

ghc Main.hs
./Main < $1.tc 1> $1.asm 2> $1.err
case "$OSTYPE" in
	darwin*)
		nasm -f macho $1.asm
		;;
	linux*)
		nasm -f elf $1.asm
		;;
esac
gcc -fno-pie -m32 -o $1 $1.o main.c
./$1
