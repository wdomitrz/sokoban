.PHONY: all clean

all: sokoban

sokoban:
	ghc -W -Wall -Wextra sokoban.hs

clean:
	rm *.hi
	rm *.o