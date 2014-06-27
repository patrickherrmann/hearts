HC = ghc

all: hearts

again: clean all

hearts: Main.hs Hearts.hs Cards.hs
	$(HC) -o hearts $^

clean:
	rm -f *.hi *.o hearts