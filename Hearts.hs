module Hearts where

import Data.List
import Data.List.Split
import Data.Ord
import Cards

--Each of the four ordered players gets a cardinal direction
data Player = N
            | E
            | S
            | W
   deriving (Show, Enum)

--Gets the player to the left
leftPlayer :: Player -> Player
leftPlayer W = N
leftPlayer p = succ p

--Gets the player across
acrossPlayer :: Player -> Player
acrossPlayer = leftPlayer . leftPlayer

--Gets the player to the right
rightPlayer :: Player -> Player
rightPlayer = acrossPlayer . leftPlayer

--Represents the passing directive for the round
data PassingPhase = PassLeft
                  | PassRight
                  | PassAcross
                  | Keep
   deriving (Show, Enum)

--Gets whatever passing phase comes next
nextPassingPhase :: PassingPhase -> PassingPhase
nextPassingPhase Keep = PassLeft
nextPassingPhase pp   = succ pp

--Deal the cards out to the players
deal :: Deck -> [(Player, Pile)]
deal = zip [N .. W] . transpose . chunksOf 4

type Score = Int
type Scoreboard = [(Player, Score)]
data Game = Game Scoreboard PassingPhase

--Everybody starts with zero points
newGame = Game (zip [N .. W] $ repeat 0) PassLeft

--Get the point worth of a card
points :: Card -> Score
points (Card Queen Spades) = 13
points (Card _ Hearts)     = 1
points _                   = 0

--Gets the scoreboard ranked by score
leaderboard :: Game -> Scoreboard
leaderboard (Game s _) = sortBy (comparing snd) s

--Look at the cards in each pile and determine the points
--Watch out for shooting the moon
score :: [Pile] -> [Score]
score piles =
   if any (26==) scores
      then map (\s -> if s == 26 then 0 else 26) scores
      else scores
   where scores = map (sum . map points) piles

--Check if the game is over
over :: Scoreboard -> Bool
over = any (>99) . map snd