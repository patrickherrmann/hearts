module Hearts where

import Data.List
import Data.List.Split
import Data.Ord
import Cards
import Control.Applicative
import Data.Maybe
import Text.Printf
import Data.Function

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
  , scores       :: PMap Int
  } deriving (Show)

data RoundState = RoundState
  { leader       :: Player
  , hands        :: PMap [Card]
  , piles        :: PMap [Card]
  , heartsBroken :: Bool
  } deriving (Show)

playGame :: IO ()
playGame = do
  let gs = initialGameState
  ws <- playRounds gs
  putStrLn $ gameOverText ws

gameOverText :: [Player] -> String
gameOverText [a] = printf "%s wins!" (show a)
gameOverText [a, b] = printf "%s and %s tie." (show a) (show b)
gameOverText [a, b, c] = printf "%s, %s, and %s tie." (show a) (show b) (show c)
gameOverText _ = "Everybody ties!"

playRounds :: GameState -> IO [Player]
playRounds gs = case winners (scores gs) of
  Nothing -> playRound gs >>= playRounds
  Just ws -> return ws

initialGameState :: GameState
initialGameState = GameState {
  passingPhase = PassLeft,
  scores = zip players $ repeat 0
}

playRound :: GameState -> IO GameState
playRound gs = do
  deck <- shuffledDeck
  let hands = deal deck
  hands' <- performPassing (passingPhase gs) hands
  let rs = initialRoundState hands'
  rs' <- playTricks rs
  let pp' = nextPassingPhase $ passingPhase gs
  let scores' = scoreRound $ piles rs'
  return $ GameState pp' scores'

initialRoundState :: PMap [Card] -> RoundState
initialRoundState hands = RoundState {
  leader = firstPlayer hands,
  hands = hands,
  piles = zip players $ repeat [],
  heartsBroken = False
}

performPassing :: PassingPhase -> PMap [Card] -> IO (PMap [Card])
performPassing Keep hands = return hands
performPassing phase hands = do
  selections <- mapM (selectPasses . snd) hands
  let (passes, keeps) = unzip selections
  let rotated = drop (passingPhaseShifts phase) $ cycle passes
  let hands' = zip players $ zipWith (++) rotated keeps
  return hands'

selectPasses :: [Card] -> IO ([Card], [Card])
selectPasses = return . splitAt 3

playTricks :: RoundState -> IO RoundState
playTricks rs = if roundOver rs
  then playTrick rs >>= playTricks
  else return rs

playTrick :: RoundState -> IO RoundState
playTrick = return

passingPhaseShifts :: PassingPhase -> Int
passingPhaseShifts PassLeft = 3
passingPhaseShifts PassAcross = 2
passingPhaseShifts PassRight = 1
passingPhaseShifts Keep = 0

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
  where deucebag (_, cs) = Card Two Clubs `elem` cs
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

winners :: PMap Int -> Maybe [Player]
winners scores = listToMaybe ws
  where wins (_, s) = s >= 100
        ws = map (map fst)
           . groupBy ((==) `on` snd)
           . sortBy (comparing (Down . snd))
           $ filter wins scores