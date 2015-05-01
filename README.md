# Hearts

The card game hearts, playable from a (currently terrible) command line interface.

Build using cabal:

```
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal build
```

Some sample play with one human player against three random players:

```
$ ./dist/build/hearts-cli/hearts-cli

Scores: -------------
N: 0
E: 0
S: 0
W: 0
Your hand:
3C 9C 3D 4D 7D 8D TD AD 2H 9H 5S 9S TS
Enter three cards to pass: 9C 3C 9H

Trick:
N: 2C

Trick:
E: 8C
N: 2C

Trick:
S: QC
E: 8C
N: 2C

Trick:
W: TC
S: QC
E: 8C
N: 2C

Trick:
S: 5C

Trick:
W: 6C
S: 5C
Your hand:
3D 4D 7D 8D TD AD 2H 5H 5S 9S TS KS
Enter a card to play: KS

Trick:
N: KS
W: 6C
S: 5C

Trick:
E: 9C
N: KS
W: 6C
S: 5C

...
```