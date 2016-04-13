# CC=ghc
# CFLAGS=-Wall
# OBJS=Filter.o Rand.o run.o

all:	
	ghc -Wall --make Brain -o brainfuck

#package:	all
#	tar cfv package.tar public web four

clean:
	-rm *.o *.hi brainfuck
