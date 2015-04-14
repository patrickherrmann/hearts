module Hearts where

import Data.List
import Data.List.Split
import Data.Ord
import Cards
import Data.Function
import qualified Data.Map as M
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Data.Random

type PMap a = M.Map Player a

data GameIO = GameIO
  { showPreRound      :: GameState -> IO ()
  , showRoundState    :: RoundState -> IO ()
  , showPostGame      :: [Player] -> IO ()
  , playerIO          :: PMap PlayerIO
  }

data PlayerIO = PlayerIO
  { getPassSelections :: [Card] -> IO (Card, Card, Card)
  , getSelectedCard   :: [Card] -> IO Card
  }

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

playGame :: GameIO -> IO ()
playGame gio = do
  let gs = initialGameState
  ws <- playRounds gio gs
  showPostGame gio ws

playRounds :: GameIO -> GameState -> IO [Player]
playRounds gio gs
  | gameOver (scores gs) = do
    showPreRound gio gs
    return . winners $ scores gs
  | otherwise = do
    showPreRound gio gs
    playRound gio gs >>= playRounds gio

initialGameState :: GameState
initialGameState = GameState {
  passingPhase = PassLeft,
  scores = M.fromList . zip players $ repeat 0
}

playRound :: GameIO -> GameState -> IO GameState
playRound gio gs = do
  deck <- shuffledDeck
  let hs = deal deck
  hs' <- performPassing gio (passingPhase gs) hs
  let rs = initialRoundState hs'
  rs' <- playFirstTrick gio rs
  rs'' <- playTricks gio rs'
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

performPassing :: GameIO -> PassingPhase -> PMap [Card] -> IO (PMap [Card])
performPassing _ Keep hs = return hs
performPassing gio phase hs = do
  selections <- T.sequence $ M.mapWithKey (selectPasses gio) hs
  let passes = M.map fst selections
  let keeps = M.map snd selections
  let shifted = M.mapKeys (passingTarget phase) passes
  return $ M.unionWith (++) shifted keeps

selectPasses :: GameIO -> Player -> [Card] -> IO ([Card], [Card])
selectPasses gio p cs = do
  let pio = playerIO gio M.! p
  (a, b, c) <- getPassSelections pio cs
  let ps = [a, b, c]
  if all (`elem` cs) ps
    then return (ps, cs \\ ps)
    else selectPasses gio p cs

playTricks :: GameIO -> RoundState -> IO RoundState
playTricks gio rs
  | roundOver rs = return rs
  | otherwise = playTrick gio rs >>= playTricks gio

playFirstTrick :: GameIO -> RoundState -> IO RoundState
playFirstTrick gio rs = do
  let rs1 = unsafePlayCard rs (toPlay rs) (Card Two Clubs)
  showRoundState gio rs1
  rs2 <- playCard gio rs1 validFirstTrickPlays
  showRoundState gio rs2
  rs3 <- playCard gio rs2 validFirstTrickPlays
  showRoundState gio rs3
  rs4 <- playCard gio rs3 validFirstTrickPlays
  showRoundState gio rs4
  return $ collectTrick rs4

selectPlay :: GameIO -> Player -> [Card] -> IO Card
selectPlay gio p hand = do
  let pio = playerIO gio M.! p
  getSelectedCard pio hand

playTrick :: GameIO -> RoundState -> IO RoundState
playTrick gio rs = do
  rs1 <- playCard gio rs validLeadCards
  let rs1' = rs1 {
    leadSuit = suit . head . M.elems $ pot rs1
  }
  showRoundState gio rs1'
  rs2 <- playCard gio rs1' validPlays
  showRoundState gio rs2
  rs3 <- playCard gio rs2 validPlays
  showRoundState gio rs3
  rs4 <- playCard gio rs3 validPlays
  showRoundState gio rs4
  return $ collectTrick rs4

type Validator = RoundState -> [Card] -> [Card]

playCard :: GameIO -> RoundState -> Validator -> IO RoundState
playCard gio rs valid = do
  let p = toPlay rs
  let h = hands rs M.! p
  card <- selectPlay gio p h
  let vs = valid rs h
  if card `elem` vs
    then return $ unsafePlayCard rs p card
    else playCard gio rs valid

unsafePlayCard :: RoundState -> Player -> Card -> RoundState
unsafePlayCard rs p c = rs {
  pot = M.insert p c (pot rs),
  hands = M.adjust (\\ [c]) p (hands rs),
  toPlay = nextPlayer p,
  heartsBroken = heartsBroken rs || (suit c == Hearts)
}

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

validPlays :: Validator
validPlays rs cs
    | null ofLeadSuit = cs
    | otherwise = ofLeadSuit
  where ofLeadSuit = filter ((== leadSuit rs) . suit) cs

validFirstTrickPlays :: Validator
validFirstTrickPlays rs cs
    | null opts = valid
    | otherwise = opts
  where notBloody = (==0) . points
        opts = filter notBloody valid
        valid = validPlays rs cs

validLeadCards :: Validator
validLeadCards rs cs
    | heartsBroken rs = cs
    | null nonHearts = cs
    | otherwise = nonHearts
  where nonHearts = filter ((/= Hearts) . suit) cs

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
shuffledDeck = sample . shuffle $ fullDeck

points :: Card -> Int
points (Card Queen Spades) = 13
points (Card _ Hearts)     = 1
points _                   = 0

roundOver :: RoundState -> Bool
roundOver = F.any null . hands

scoreRound :: PMap [Card] -> PMap Int
scoreRound = adjustIfMoonShot . M.map (sum . map points)

adjustIfMoonShot :: PMap Int -> PMap Int
adjustIfMoonShot ss
    | F.any (==26) ss = M.map newScore ss
    | otherwise = ss
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