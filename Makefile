all:	
	ghc -Wall --make Brainfuck -o brainfuck

clean:
	-rm *.o *.hi brainfuck
