Goat: GoatParser.hs GoatAST.hs Goat.hs
	ghc Goat.hs -o Goat

clean:
	rm -f *.o *.hi
	rm -f Goat