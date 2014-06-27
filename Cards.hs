module Cards where

--Card ranks; Aces are high
data Rank = Two
          | Three
          | Four
          | Five
          | Six
          | Seven
          | Eight
          | Nine
          | Ten
          | Jack
          | Queen
          | King
          | Ace
   deriving (Show, Eq, Ord, Enum)

--Card suits
data Suit = Clubs
          | Diamonds
          | Hearts
          | Spades
   deriving (Show, Eq, Enum)

--Playing cards; no jokers
data Card = Card Rank Suit

--Different names for a list of cards
type Pile = [Card]
type Deck = Pile
type Hand = Pile

--Provides a full deck
fullDeck :: Deck
fullDeck = [Card r s | r <- [Two .. Ace], s <- [Clubs .. Spades]]

--Long name of a card
instance Show Card where
   show (Card r s) = (show r) ++ " of " ++ (show s)

--Shorthand is an abridged string representation
class Shorthand a where
   shorthand :: a -> String

--One character representation
instance Shorthand Rank where
  shorthand Two   = "2"
  shorthand Three = "3"
  shorthand Four  = "4"
  shorthand Five  = "5"
  shorthand Six   = "6"
  shorthand Seven = "7"
  shorthand Eight = "8"
  shorthand Nine  = "9"
  shorthand r     = take 1 . show $ r

--One character representation
instance Shorthand Suit where
  shorthand = take 1 . show

--Two character representation
instance Shorthand Card where
  shorthand (Card r s) = (shorthand r) ++ (shorthand s)