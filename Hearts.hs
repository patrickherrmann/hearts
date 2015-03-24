module Hearts where

import Data.List
import Data.List.Split
import Data.Ord
import Cards
import Control.Applicative
import Data.Maybe
import Text.Printf
import Data.Function
import qualified Data.Map as M
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Control.Monad.Random
import System.Random.Shuffle

type PMap a = M.Map Player a

data Player
  = N
  | E
  | S
  | W
  deriving (Show, Eq, Enum, Ord)

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
  { toPlay       :: Player
  , leadSuit     :: Suit
  , hands        :: PMap [Card]
  , piles        :: PMap [Card]
  , pot          :: PMap Card
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
playRounds gs
  | gameOver (scores gs) = return . winners $ scores gs
  | otherwise = playRound gs >>= playRounds

initialGameState :: GameState
initialGameState = GameState {
  passingPhase = PassLeft,
  scores = M.fromList . zip players $ repeat 0
}

playRound :: GameState -> IO GameState
playRound gs = do
  deck <- shuffledDeck
  let hs = deal deck
  hs' <- performPassing (passingPhase gs) hs
  let rs = initialRoundState hs'
  rs' <- playFirstTrick rs
  rs'' <- playTricks rs'
  return GameState {
    passingPhase = nextPassingPhase $ passingPhase gs,
    scores = M.unionWith (+) (scores gs) (scoreRound $ piles rs'')
  }

initialRoundState :: PMap [Card] -> RoundState
initialRoundState hs = RoundState {
  toPlay = firstPlayer hs,
  leadSuit = Clubs,
  hands = hs,
  piles = M.fromList . zip players $ repeat [],
  pot = M.empty,
  heartsBroken = False
}

performPassing :: PassingPhase -> PMap [Card] -> IO (PMap [Card])
performPassing Keep hands = return hands
performPassing phase hands = do
  selections <- T.sequence $ M.mapWithKey selectPasses hands
  let passes = M.map fst selections
  let keeps = M.map snd selections
  let shifted = M.mapKeys (passingTarget phase) passes
  return $ M.unionWith (++) shifted keeps

selectPasses :: Player -> [Card] -> IO ([Card], [Card])
selectPasses p cs = return $ splitAt 3 cs

playTricks :: RoundState -> IO RoundState
playTricks rs
  | roundOver rs = return rs
  | otherwise = playTrick rs >>= playTricks

playFirstTrick :: RoundState -> IO RoundState
playFirstTrick rs = do
  let rs1 = unsafePlayCard rs (toPlay rs) (Card Two Clubs)
  rs2 <- playCard rs1 validFirstTrickPlays
  rs3 <- playCard rs2 validFirstTrickPlays
  rs4 <- playCard rs3 validFirstTrickPlays
  return $ collectTrick rs4

collectTrick :: RoundState -> RoundState
collectTrick rs = rs {
    toPlay = w,
    pot = M.empty,
    piles = M.adjust (++ M.elems (pot rs)) w (piles rs)
  }
  where w = trickWinner rs

trickWinner :: RoundState -> Player
trickWinner rs = fst
               . maximumBy (comparing (rank . snd))
               . filter ((== leadSuit rs) . suit . snd)
               $ M.assocs (pot rs)

validPlays :: RoundState -> [Card] -> [Card]
validPlays rs cs
    | null ofLeadSuit = cs
    | otherwise = ofLeadSuit
  where ofLeadSuit = filter ((== leadSuit rs) . suit) cs

validFirstTrickPlays :: RoundState -> [Card] -> [Card]
validFirstTrickPlays rs cs
    | null opts = valid
    | otherwise = opts
  where notBloody = (==0) . points
        opts = filter notBloody valid
        valid = validPlays rs cs

selectPlay :: Player -> [Card] -> IO Card
selectPlay p hand = return $ head hand

playTrick :: RoundState -> IO RoundState
playTrick rs = do
  rs1 <- playCard rs validLeadCards
  rs2 <- playCard rs1 validPlays
  rs3 <- playCard rs2 validPlays
  rs4 <- playCard rs3 validPlays
  return $ collectTrick rs4

validLeadCards :: RoundState -> [Card] -> [Card]
validLeadCards rs cs
  | heartsBroken rs = cs
  | otherwise = filter ((/= Hearts) . suit) cs

playCard :: RoundState -> (RoundState -> [Card] -> [Card]) -> IO RoundState
playCard rs valid = do
  let p = toPlay rs
  let h = hands rs M.! p
  card <- selectPlay p h
  let vs = valid rs h
  if card `elem` vs
    then return $ unsafePlayCard rs p card
    else playCard rs valid

unsafePlayCard :: RoundState -> Player -> Card -> RoundState
unsafePlayCard rs p c = rs {
  pot = M.insert p c (pot rs),
  hands = M.adjust (\\ [c]) p (hands rs),
  toPlay = nextPlayer p,
  heartsBroken = heartsBroken rs || (suit c == Hearts)
}

passingTarget :: PassingPhase -> Player -> Player
passingTarget Keep = id
passingTarget PassLeft = nextPlayer
passingTarget PassAcross = nextPlayer . nextPlayer
passingTarget PassRight = nextPlayer . nextPlayer . nextPlayer

nextPassingPhase :: PassingPhase -> PassingPhase
nextPassingPhase Keep = PassLeft
nextPassingPhase pp   = succ pp

players :: [Player]
players = [N .. W]

nextPlayer :: Player -> Player
nextPlayer W = N
nextPlayer p = succ p

firstPlayer :: PMap [Card] -> Player
firstPlayer = head . M.keys . M.filter (Card Two Clubs `elem`)

deal :: [Card] -> PMap [Card]
deal = M.fromList . zip players . transpose . chunksOf 4

shuffledDeck :: IO [Card]
shuffledDeck = evalRandIO $ shuffleM fullDeck

points :: Card -> Int
points (Card Queen Spades) = 13
points (Card _ Hearts)     = 1
points _                   = 0

roundOver :: RoundState -> Bool
roundOver = F.any null . hands

scoreRound :: PMap [Card] -> PMap Int
scoreRound = adjustIfMoonShot . M.map (sum . map points)

adjustIfMoonShot :: PMap Int -> PMap Int
adjustIfMoonShot scores
    |  M.null $ M.map (==26) scores = scores
    | otherwise = M.map newScore scores
  where newScore 26 = 0
        newScore _  = 26

winners :: PMap Int -> [Player]
winners = map fst
        . head
        . groupBy ((==) `on` snd)
        . sortBy (comparing snd)
        . M.assocs

gameOver :: PMap Int -> Bool
gameOver = F.any (>= 100)