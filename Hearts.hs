module Hearts where

import Data.List
import Data.List.Split
import Data.Ord
import Cards

type PlayerMap a = [(Player, a)]

data Player
  = N
  | E
  | S
  | W
  deriving (Show, Enum)

data PassingPhase
  = PassLeft
  | PassRight
  | PassAcross
  | Keep
  deriving (Show, Enum)

data GameState = GameState
  { passingPhase :: PassingPhase
  , scoreBoard :: PlayerMap Int
  } deriving (Show)

data RoundState = RoundState
  { leader :: Player
  , hands  :: PlayerMap [Card]
  } deriving (Show)

playRound :: GameState -> GameState
playRound = id

leftPlayer :: Player -> Player
leftPlayer W = N
leftPlayer p = succ p

acrossPlayer :: Player -> Player
acrossPlayer = leftPlayer . leftPlayer

rightPlayer :: Player -> Player
rightPlayer = acrossPlayer . leftPlayer

getReceiver :: PassingPhase -> Player -> Player
getReceiver PassLeft = leftPlayer
getReceiver PassRight = rightPlayer
getReceiver PassAcross = acrossPlayer
getReceiver Keep = id

getPasser :: PassingPhase -> Player -> Player
getPasser PassLeft = rightPlayer
getPasser PassRight = leftPlayer
getPasser PassAcross = acrossPlayer
getPasser Keep = id

nextPassingPhase :: PassingPhase -> PassingPhase
nextPassingPhase Keep = PassLeft
nextPassingPhase pp   = succ pp

deal :: [Card] -> PlayerMap [Card]
deal = zip [N .. W] . transpose . chunksOf 4

points :: Card -> Int
points (Card Queen Spades) = 13
points (Card _ Hearts)     = 1
points _                   = 0