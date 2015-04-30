module Cards
  ( Rank(..)
  , Suit(..)
  , Card(..)
  , fullDeck
  , showUnicode
  , readCard
  ) where

import Control.Applicative
import Data.List
import Data.List.Split

data Rank
  = Two
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
  deriving (Eq, Enum, Ord)

data Suit
  = Clubs
  | Diamonds
  | Hearts
  | Spades
   deriving (Eq, Enum, Ord)

data Card = Card
  { rank :: Rank
  , suit :: Suit
  } deriving (Eq)

fullDeck :: [Card]
fullDeck = Card <$> [Two .. Ace] <*> [Clubs .. Spades]

instance Show Rank where
  show Two   = "2"
  show Three = "3"
  show Four  = "4"
  show Five  = "5"
  show Six   = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine  = "9"
  show Ten   = "T"
  show Jack  = "J"
  show Queen = "Q"
  show King  = "K"
  show Ace   = "A"

instance Show Suit where
  show Clubs = "C"
  show Diamonds = "D"
  show Hearts = "H"
  show Spades = "S"

instance Show Card where
  show (Card r s) = show r ++ show s

unicode :: String
unicode = "🃒🃓🃔🃕🃖🃗🃘🃙🃚🃛🃝🃞🃑\
          \🃂🃃🃄🃅🃆🃇🃈🃉🃊🃋🃍🃎🃁\
          \🂲🂳🂴🂵🂶🂷🂸🂹🂺🂻🂽🂾🂱\
          \🂢🂣🂤🂥🂦🂧🂨🂩🂪🂫🂭🂮🂡"

unicodeMapping :: [(Card, Char)]
unicodeMapping = zip fullDeck (concat . transpose . chunksOf 13 $ unicode)

showUnicode :: Card -> String
showUnicode c = [u]
  where Just u = lookup c unicodeMapping

readCard :: String -> Maybe Card
readCard "2C" = Just $ Card Two Clubs
readCard "3C" = Just $ Card Three Clubs
readCard "4C" = Just $ Card Four Clubs
readCard "5C" = Just $ Card Five Clubs
readCard "6C" = Just $ Card Six Clubs
readCard "7C" = Just $ Card Seven Clubs
readCard "8C" = Just $ Card Eight Clubs
readCard "9C" = Just $ Card Nine Clubs
readCard "TC" = Just $ Card Ten Clubs
readCard "JC" = Just $ Card Jack Clubs
readCard "QC" = Just $ Card Queen Clubs
readCard "KC" = Just $ Card King Clubs
readCard "AC" = Just $ Card Ace Clubs
readCard "2D" = Just $ Card Two Diamonds
readCard "3D" = Just $ Card Three Diamonds
readCard "4D" = Just $ Card Four Diamonds
readCard "5D" = Just $ Card Five Diamonds
readCard "6D" = Just $ Card Six Diamonds
readCard "7D" = Just $ Card Seven Diamonds
readCard "8D" = Just $ Card Eight Diamonds
readCard "9D" = Just $ Card Nine Diamonds
readCard "TD" = Just $ Card Ten Diamonds
readCard "JD" = Just $ Card Jack Diamonds
readCard "QD" = Just $ Card Queen Diamonds
readCard "KD" = Just $ Card King Diamonds
readCard "AD" = Just $ Card Ace Diamonds
readCard "2H" = Just $ Card Two Hearts
readCard "3H" = Just $ Card Three Hearts
readCard "4H" = Just $ Card Four Hearts
readCard "5H" = Just $ Card Five Hearts
readCard "6H" = Just $ Card Six Hearts
readCard "7H" = Just $ Card Seven Hearts
readCard "8H" = Just $ Card Eight Hearts
readCard "9H" = Just $ Card Nine Hearts
readCard "TH" = Just $ Card Ten Hearts
readCard "JH" = Just $ Card Jack Hearts
readCard "QH" = Just $ Card Queen Hearts
readCard "KH" = Just $ Card King Hearts
readCard "AH" = Just $ Card Ace Hearts
readCard "2S" = Just $ Card Two Spades
readCard "3S" = Just $ Card Three Spades
readCard "4S" = Just $ Card Four Spades
readCard "5S" = Just $ Card Five Spades
readCard "6S" = Just $ Card Six Spades
readCard "7S" = Just $ Card Seven Spades
readCard "8S" = Just $ Card Eight Spades
readCard "9S" = Just $ Card Nine Spades
readCard "TS" = Just $ Card Ten Spades
readCard "JS" = Just $ Card Jack Spades
readCard "QS" = Just $ Card Queen Spades
readCard "KS" = Just $ Card King Spades
readCard "AS" = Just $ Card Ace Spades
readCard _ = Nothing