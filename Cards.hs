module Cards where

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

data Suit = Clubs
          | Diamonds
          | Hearts
          | Spades
   deriving (Show, Eq, Ord, Enum)

data Card = Card Rank Suit

instance Show Card where
   show (Card r s) = (show r) ++ " of " ++ (show s)

fullDeck = [Card r s | r <- [Two .. Ace], s <- [Clubs .. Spades]]

type Pile = [Card]
type Deck = Pile

--Shorthand for shorter printing of cards

class Shorthand a where
   shorthand :: a -> String

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

instance Shorthand Suit where
  shorthand = take 1 . show

instance Shorthand Card where
  shorthand (Card r s) = (shorthand r) ++ (shorthand s)