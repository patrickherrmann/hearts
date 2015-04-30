{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hearts
  ( GameIO(..)
  , PlayerIO(..)
  , Player(..)
  , PassingPhase(..)
  , MoveInfraction(..)
  , GameState(..)
  , RoundStateView(..)
  , playGame
  ) where

import Data.List
import Data.List.Split
import Data.Ord
import Cards
import Data.Monoid
import Control.Applicative
import Data.Function
import qualified Data.Map as M
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Data.Random
import Control.Monad.Reader

newtype HeartsIO a = HeartsIO
  { runHearts :: ReaderT GameIO IO a
  } deriving
  ( Functor
  , Applicative
  , Monad
  , MonadReader GameIO
  , MonadIO
  )

type PMap a = M.Map Player a
type PlayValidation = RoundState -> Card -> Maybe MoveInfraction

data GameIO = GameIO
  { playerIO          :: PMap PlayerIO
  }

data PlayerIO = PlayerIO
  { getPassSelections :: [Card] -> IO (Card, Card, Card)
  , getSelectedCard   :: RoundStateView -> IO Card
  , receiveFeedback   :: MoveInfraction -> IO ()
  , showPreRound      :: GameState -> IO ()
  , showRoundState    :: RoundStateView -> IO ()
  , showPostGame      :: [Player] -> IO ()
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

data MoveInfraction
  = CardNotInHand Card
  | MustPlayLeadSuit Suit
  | HeartsNotBroken
  | NoPointsFirstTrick

data GameState = GameState
  { passingPhase :: PassingPhase
  , scores       :: PMap Int
  } deriving (Show)

data RoundState = RoundState
  { toPlay       :: Player
  , leadSuit     :: Suit
  , hands        :: PMap [Card]
  , piles        :: PMap [Card]
  , pot          :: [(Player, Card)]
  , heartsBroken :: Bool
  } deriving (Show)

data RoundStateView = RoundStateView
  { handView :: [Card]
  , potView :: [(Player, Card)]
  }

runHeartsWith :: GameIO -> HeartsIO a -> IO a
runHeartsWith gio = flip runReaderT gio . runHearts

doPlayerIO :: Player -> (PlayerIO -> a -> IO b) -> a -> HeartsIO b
doPlayerIO p f a = do
  pio <- (M.! p) <$> asks playerIO
  liftIO $ f pio a

doEachPlayerIO_ :: (PlayerIO -> a -> IO b) -> (Player -> a) -> HeartsIO ()
doEachPlayerIO_ f fa = do
  pios <- M.assocs <$> asks playerIO
  let res (p, pio) = f pio (fa p)
  liftIO $ mapM_ res pios

playGame :: GameIO -> IO ()
playGame gio =  runHeartsWith gio $ playRounds initialGameState

playRounds :: GameState -> HeartsIO ()
playRounds gs
  | gameOver (scores gs) = do
    doEachPlayerIO_ showPreRound (const gs)
    let ws = winners $ scores gs
    doEachPlayerIO_ showPostGame (const ws)
  | otherwise = do
    doEachPlayerIO_ showPreRound (const gs)
    playRound gs >>= playRounds

initialGameState :: GameState
initialGameState = GameState {
  passingPhase = PassLeft,
  scores = M.fromList . zip players $ repeat 0
}

playRound :: GameState -> HeartsIO GameState
playRound gs = do
  deck <- liftIO shuffledDeck
  let hs = deal deck
  hs' <- performPassing (passingPhase gs) hs
  rs <- playFirstTrick (initialRoundState hs') >>= playTricks
  return GameState {
    passingPhase = nextPassingPhase $ passingPhase gs,
    scores = M.unionWith (+) (scores gs) (scoreRound $ piles rs)
  }

initialRoundState :: PMap [Card] -> RoundState
initialRoundState hs = RoundState {
  toPlay = firstPlayer hs,
  leadSuit = Clubs,
  hands = hs,
  piles = M.fromList . zip players $ repeat [],
  pot = [],
  heartsBroken = False
}

performPassing :: PassingPhase -> PMap [Card] -> HeartsIO (PMap [Card])
performPassing Keep hs = return hs
performPassing phase hs = do
  selections <- T.sequence $ M.mapWithKey selectPasses hs
  let passes = M.map fst selections
  let keeps = M.map snd selections
  let shifted = M.mapKeys (passingTarget phase) passes
  return $ M.unionWith (++) shifted keeps

selectPasses :: Player -> [Card] -> HeartsIO ([Card], [Card])
selectPasses p cs = do
  (x, y, z) <- doPlayerIO p getPassSelections cs
  let ps = [x, y, z]
  let cardInHand c
        | c `notElem` cs = Just $ CardNotInHand c
        | otherwise = Nothing
  let (First mErr) = mconcat $ map (First . cardInHand) ps
  case mErr of
    Nothing -> return (ps, cs \\ ps)
    Just err -> do
      doPlayerIO p receiveFeedback err
      selectPasses p cs

playTricks :: RoundState -> HeartsIO RoundState
playTricks rs
  | roundOver rs = return rs
  | otherwise = playTrick rs >>= playTricks

leadFirstTrick :: RoundState -> HeartsIO RoundState
leadFirstTrick rs = do
  let rs' = unsafePlayCard rs (toPlay rs) (Card Two Clubs)
  doEachPlayerIO_ showRoundState (getRoundStateView rs')
  return rs'

continueFirstTrick :: RoundState -> HeartsIO RoundState
continueFirstTrick = progressRound validContinueFirstTrick

playFirstTrick :: RoundState -> HeartsIO RoundState
playFirstTrick = leadFirstTrick
  >=> continueFirstTrick
  >=> continueFirstTrick
  >=> continueFirstTrick
  >=> return . collectTrick

leadTrick :: RoundState -> HeartsIO RoundState
leadTrick rs = do
  rs' <- progressRound validLeadTrick rs
  return $ rs' {
    leadSuit = suit . snd . head $ pot rs'
  }

continueTrick :: RoundState -> HeartsIO RoundState
continueTrick = progressRound validContinueTrick

playTrick :: RoundState -> HeartsIO RoundState
playTrick = leadTrick
  >=> continueTrick
  >=> continueTrick
  >=> continueTrick
  >=> return . collectTrick

progressRound :: [PlayValidation] -> RoundState -> HeartsIO RoundState
progressRound vs rs = do
  let p = toPlay rs
  c <- doPlayerIO p getSelectedCard (getRoundStateView rs p)
  let (First mErr) = mconcat $ map (\v -> First $ v rs c) vs
  case mErr of
    Just err -> do
      doPlayerIO p receiveFeedback err
      progressRound vs rs
    Nothing  -> do
      let rs' = unsafePlayCard rs (toPlay rs) c
      doEachPlayerIO_ showRoundState (getRoundStateView rs')
      return rs'

unsafePlayCard :: RoundState -> Player -> Card -> RoundState
unsafePlayCard rs p c = rs {
  pot = (p, c) : (pot rs),
  hands = M.adjust (\\ [c]) p (hands rs),
  toPlay = nextPlayer p,
  heartsBroken = heartsBroken rs || suit c == Hearts
}

getRoundStateView :: RoundState -> Player -> RoundStateView
getRoundStateView rs p = RoundStateView {
  handView = hands rs M.! p,
  potView = pot rs
}

collectTrick :: RoundState -> RoundState
collectTrick rs = rs {
    toPlay = w,
    pot = [],
    piles = M.adjust (++ map snd (pot rs)) w (piles rs)
  }
  where w = trickWinner rs

trickWinner :: RoundState -> Player
trickWinner rs = fst
               . maximumBy (comparing (rank . snd))
               . filter ((== leadSuit rs) . suit . snd)
               $ pot rs

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

handToPlay :: RoundState -> [Card]
handToPlay rs = hands rs M.! toPlay rs

deal :: [Card] -> PMap [Card]
deal = M.fromList . zip players . transpose . chunksOf 4

shuffledDeck :: IO [Card]
shuffledDeck = sample . shuffle $ fullDeck

points :: Card -> Int
points (Card Queen Spades) = 13
points (Card _ Hearts)     = 1
points _                   = 0

worthPoints :: Card -> Bool
worthPoints = (>0) . points

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

hasCardInHand :: PlayValidation
hasCardInHand rs c
  | c `elem` handToPlay rs = Nothing
  | otherwise = Just $ CardNotInHand c

playingLeadSuit :: PlayValidation
playingLeadSuit rs c
  | suit c == ls || all ((/= ls) . suit) (handToPlay rs) = Nothing
  | otherwise = Just $ MustPlayLeadSuit ls
  where ls = leadSuit rs

notLeadingUnbrokenHearts :: PlayValidation
notLeadingUnbrokenHearts rs c
  | heartsBroken rs || suit c /= Hearts || all ((== Hearts) . suit) (handToPlay rs) = Nothing
  | otherwise = Just HeartsNotBroken

notPlayingPointCards :: PlayValidation
notPlayingPointCards rs c
  | not $ worthPoints c || all worthPoints (handToPlay rs) = Nothing
  | otherwise = Just NoPointsFirstTrick

validLeadTrick :: [PlayValidation]
validLeadTrick = [hasCardInHand, notLeadingUnbrokenHearts]

validContinueTrick :: [PlayValidation]
validContinueTrick = [hasCardInHand, playingLeadSuit]

validContinueFirstTrick :: [PlayValidation]
validContinueFirstTrick = [hasCardInHand, playingLeadSuit, notPlayingPointCards]