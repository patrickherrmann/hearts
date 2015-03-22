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
  { leader       :: Player
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
  rs' <- playTricks rs
  return GameState {
    passingPhase = nextPassingPhase $ passingPhase gs,
    scores = M.unionWith (+) (scores gs) (scoreRound $ piles rs')
  }

initialRoundState :: PMap [Card] -> RoundState
initialRoundState hs = RoundState {
  leader = firstPlayer hs,
  leadSuit = Clubs,
  hands = hs,
  piles = M.fromList . zip players $ repeat [],
  pot = M.empty,
  heartsBroken = False
}

performPassing :: PassingPhase -> PMap [Card] -> IO (PMap [Card])
performPassing Keep hands = return hands
performPassing phase hands = do
  let (ps, hs) = unzip $ M.assocs hands
  selections <- mapM selectPasses hs
  let (passes, keeps) = unzip selections
  let rotated = drop (passingPhaseShifts phase) $ cycle passes
  return . M.fromList . zip ps $ zipWith (++) rotated keeps

selectPasses :: [Card] -> IO ([Card], [Card])
selectPasses = return . splitAt 3

playTricks :: RoundState -> IO RoundState
playTricks rs
  | roundOver rs = return rs
  | otherwise = playTrick rs >>= playTricks

playFirstTrick :: RoundState -> IO RoundState
playFirstTrick rs = do
  let rs' = unsafePlayCard rs (leader rs) (Card Two Clubs)
  return rs'

playCardFirstTrick :: RoundState -> Player -> IO RoundState
playCardFirstTrick rs p = do
  let h = hands rs M.! p
  card <- selectPlay p h
  let vs = validFirstTrickPlays rs h
  if card `elem` vs
    then return $ unsafePlayCard rs p card
    else playCardFirstTrick rs p



validPlays :: RoundState -> [Card] -> [Card]
validPlays rs cs
    | not . null $ ofLeadSuit = ofLeadSuit
    | otherwise = cs
  where ofLeadSuit = filter ((==(leadSuit rs)) . suit) cs

validFirstTrickPlays :: RoundState -> [Card] -> [Card]
validFirstTrickPlays rs cs
    | null opts = valid
  where notBloody = (==0) . points
        opts = filter notBloody valid
        valid = validPlays rs cs

selectPlay :: Player -> [Card] -> IO Card
selectPlay p hand = return $ head hand

playTrick :: RoundState -> IO RoundState
playTrick = return

unsafePlayCard :: RoundState -> Player -> Card -> RoundState
unsafePlayCard rs p c = rs {
  pot = M.singleton p c,
  hands = M.adjust (\\ [c]) (leader rs) (hands rs)
}

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

nextPlayer :: Player -> Player
nextPlayer W = N
nextPlayer p = succ p

firstPlayer :: PMap [Card] -> Player
firstPlayer = head . M.keys . M.filter (Card Two Clubs `elem`)

deal :: [Card] -> PMap [Card]
deal = M.fromList . zip players . transpose . chunksOf 4

shuffledDeck :: IO [Card]
shuffledDeck = return fullDeck

points :: Card -> Int
points (Card Queen Spades) = 13
points (Card _ Hearts)     = 1
points _                   = 0

roundOver :: RoundState -> Bool
roundOver = any null . M.elems . hands

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
gameOver = not . M.null . M.filter (>= 100)