Goat: CodeGenerate.hs GoatAST.hs GoatParser.hs GoatPrettyPrinter.hs Goat.hs
	ghc Goat.hs -o Goat

clean:
	rm -f *.o *.hi
	rm -f Goat