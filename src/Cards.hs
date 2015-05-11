module Cards
  ( Rank(..)
  , Suit(..)
  , Card(..)
  , fullDeck
  , showUnicode
  , readCard
  ) where

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

readRank :: Char -> Maybe Rank
readRank '2' = Just Two
readRank '3' = Just Three
readRank '4' = Just Four
readRank '5' = Just Five
readRank '6' = Just Six
readRank '7' = Just Seven
readRank '8' = Just Eight
readRank '9' = Just Nine
readRank 'T' = Just Ten
readRank 'J' = Just Jack
readRank 'Q' = Just Queen
readRank 'K' = Just King
readRank 'A' = Just Ace
readRank  _  = Nothing

readSuit :: Char -> Maybe Suit
readSuit 'C' = Just Clubs
readSuit 'D' = Just Diamonds
readSuit 'H' = Just Hearts
readSuit 'S' = Just Spades
readSuit  _  = Nothing

readCard :: String -> Maybe Card
readCard [r, s] = Card <$> readRank r <*> readSuit s
readCard _ = Nothing