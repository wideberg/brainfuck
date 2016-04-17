all:	
	ghc -Wall --make Brain -o brainfuck

clean:
	-rm *.o *.hi brainfuck
