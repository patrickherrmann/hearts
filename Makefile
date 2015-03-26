HC = ghc
FLAGS=-Wall

all: hearts

again: clean all

hearts: Main.hs Hearts.hs Cards.hs
	$(HC) $(FLAGS) -o hearts $^

clean:
	rm -f *.hi *.o hearts