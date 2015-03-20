module Hearts where

import Data.List
import Data.List.Split
import Data.Ord
import Cards
import Control.Applicative

type PMap a = [(Player, a)]

data Player
  = N
  | E
  | S
  | W
  deriving (Show, Eq, Enum)

data PassingPhase
  = PassLeft
  | PassRight
  | PassAcross
  | Keep
  deriving (Show, Enum)

data GameState = GameState
  { passingPhase :: PassingPhase
  , scores :: PMap Int
  } deriving (Show)

data RoundState = RoundState
  { leader :: Player
  , hands  :: PMap [Card]
  , piles  :: PMap [Card]
  } deriving (Show)

playRound :: GameState -> IO GameState
playRound gs = do
  deck <- shuffledDeck
  let hands = deal deck
  hands' <- performPassing (passingPhase gs) hands
  let emptyPiles = zip players $ repeat []
  let rs = RoundState (firstPlayer hands') hands' emptyPiles
  rs' <- playTricks rs
  let pp' = nextPassingPhase $ passingPhase gs
  let scores' = scoreRound (piles rs')
  return $ GameState pp' scores'

playTricks :: RoundState -> IO RoundState
playTricks rs = do
  rs' <- playTrick rs
  if roundOver rs'
    then return rs'
    else playTricks rs'

performPassing :: PassingPhase -> PMap [Card] -> IO (PMap [Card])
performPassing phase hands = return hands

playTrick :: RoundState -> IO RoundState
playTrick = return

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

players :: [Player]
players = [N .. W]

deal :: [Card] -> PMap [Card]
deal = zip players . transpose . chunksOf 4

shuffledDeck :: IO [Card]
shuffledDeck = return fullDeck

firstPlayer :: PMap [Card] -> Player
firstPlayer hands = p
  where deucebag (_, cs) = (Card Two Clubs) `elem` cs
        Just (p, _) = find deucebag hands

points :: Card -> Int
points (Card Queen Spades) = 13
points (Card _ Hearts)     = 1
points _                   = 0

roundOver :: RoundState -> Bool
roundOver = null . snd . head . hands

scoreRound :: PMap [Card] -> PMap Int
scoreRound = adjustIfMoonShot . map (fmap $ sum . map points)

adjustIfMoonShot :: PMap Int -> PMap Int
adjustIfMoonShot scores = case shooter of
    Nothing -> scores
    Just (p, _) -> zip players $ map (newScore p) players
  where shooter = find ((==26) . snd) scores
        newScore p p' = if p == p' then 0 else 26

addScores :: PMap Int -> PMap Int -> PMap Int
addScores a b = zip players $ map score' players
  where score' p = let Just aScore = lookup p a
                       Just bScore = lookup p b
                   in aScore + bScore