{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}

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
import Control.Applicative
import Data.Function
import qualified Data.Map as M
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
type Validator = RoundState -> Card -> Maybe MoveInfraction

data GameIO = GameIO
  { playerIO              :: PMap PlayerIO
  }

data PlayerIO = PlayerIO
  { choosePassSelections  :: [Card] -> IO [Card]
  , chooseCard            :: RoundStateView -> IO Card
  , showMoveInfraction    :: MoveInfraction -> IO ()
  , showGameState         :: GameState -> IO ()
  , showRoundState        :: RoundStateView -> IO ()
  , showWinners           :: [Player] -> IO ()
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
  | MustPassExactlyThreeCards

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

playGame :: GameIO -> IO ()
playGame gio = runHeartsWith gio entireGame

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

entireGame :: HeartsIO ()
entireGame = do
  let gs = initialGameState
  doEachPlayerIO_ showGameState (const gs)
  gs' <- playRounds gs
  let ws = winners $ scores gs'
  doEachPlayerIO_ showWinners (const ws)

playRounds :: GameState -> HeartsIO GameState
playRounds gs
  | gameOver (scores gs) = return gs
  | otherwise = playRound gs >>= playRounds

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
  let gs' = GameState {
    passingPhase = nextPassingPhase $ passingPhase gs,
    scores = M.unionWith (+) (scores gs) (scoreRound $ piles rs)
  }
  doEachPlayerIO_ showGameState (const gs')
  return gs'

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
  selections <- sequence $ M.mapWithKey selectPasses hs
  let passes = M.map fst selections
  let keeps = M.map snd selections
  let shifted = M.mapKeys (passingTarget phase) passes
  return $ M.unionWith (++) shifted keeps

selectPasses :: Player -> [Card] -> HeartsIO ([Card], [Card])
selectPasses p cs = do
  ps <- doPlayerIO p choosePassSelections cs
  case validPasses cs ps of
    Nothing -> return (ps, cs \\ ps)
    Just err -> doPlayerIO p showMoveInfraction err >> selectPasses p cs

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

progressRound :: Validator -> RoundState -> HeartsIO RoundState
progressRound vf rs = do
  let p = toPlay rs
  c <- doPlayerIO p chooseCard (getRoundStateView rs p)
  let mErr = vf rs c
  case mErr of
    Just err -> do
      doPlayerIO p showMoveInfraction err
      progressRound vf rs
    Nothing  -> do
      let rs' = unsafePlayCard rs (toPlay rs) c
      doEachPlayerIO_ showRoundState (getRoundStateView rs')
      return rs'

unsafePlayCard :: RoundState -> Player -> Card -> RoundState
unsafePlayCard rs p c = rs {
  pot = (p, c) : pot rs,
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
roundOver = any null . hands

scoreRound :: PMap [Card] -> PMap Int
scoreRound = adjustIfMoonShot . M.map (sum . map points)

adjustIfMoonShot :: PMap Int -> PMap Int
adjustIfMoonShot ss
    | any (==26) ss = M.map newScore ss
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
gameOver = any (>= 100)

(<||>) :: Validator -> Validator -> Validator
a <||> b = \rs c -> a rs c <|> b rs c

cardInHand :: [Card] -> Card -> Maybe MoveInfraction
cardInHand cs c
  | c `elem` cs = Nothing
  | otherwise = Just $ CardNotInHand c

hasCardInHand :: Validator
hasCardInHand rs = cardInHand (handToPlay rs)

playingLeadSuit :: Validator
playingLeadSuit rs c
  | suit c == ls || all ((/= ls) . suit) (handToPlay rs) = Nothing
  | otherwise = Just $ MustPlayLeadSuit ls
  where ls = leadSuit rs

notLeadingUnbrokenHearts :: Validator
notLeadingUnbrokenHearts rs c
  | heartsBroken rs || suit c /= Hearts || all ((== Hearts) . suit) (handToPlay rs) = Nothing
  | otherwise = Just HeartsNotBroken

notPlayingPointCards :: Validator
notPlayingPointCards rs c
  | not $ worthPoints c || all worthPoints (handToPlay rs) = Nothing
  | otherwise = Just NoPointsFirstTrick

validLeadTrick :: Validator
validLeadTrick = hasCardInHand <||> notLeadingUnbrokenHearts

validContinueTrick :: Validator
validContinueTrick = hasCardInHand <||> playingLeadSuit

validContinueFirstTrick :: Validator
validContinueFirstTrick = hasCardInHand <||> playingLeadSuit <||> notPlayingPointCards

validPasses :: [Card] -> [Card] -> Maybe MoveInfraction
validPasses cs (nub -> [x, y, z]) = cardInHand cs x <|> cardInHand cs y <|> cardInHand cs z
validPasses _ _ = Just MustPassExactlyThreeCards