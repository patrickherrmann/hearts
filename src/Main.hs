import Cards
import Hearts
import Text.Printf
import qualified Data.Map as M
import Data.Random
import Data.List
import Control.Applicative
import System.IO
import Data.Ord
import Data.Monoid

showPMap :: [(Player, a)] -> (a -> String) -> IO ()
showPMap pmap toS = mapM_ (putStrLn . showP) pmap
    where showP (p, a) = show p ++ ": " ++ toS a

printPreRound :: GameState -> IO ()
printPreRound gs = do
    putStrLn "\nScores: -------------"
    showPMap (M.assocs $ scores gs) show

printRoundState :: RoundState -> IO ()
printRoundState rs = do
    putStrLn "\nTrick:"
    showPMap (pot rs) show

printPostGame :: [Player] -> IO ()
printPostGame = putStrLn . gameOverText

gameOverText :: [Player] -> String
gameOverText [a] = printf "%s wins!" (show a)
gameOverText [a, b] = printf "%s and %s tie." (show a) (show b)
gameOverText [a, b, c] = printf "%s, %s, and %s tie." (show a) (show b) (show c)
gameOverText _ = "Everybody ties!"

firstThreeCards :: [Card] -> IO (Card, Card, Card)
firstThreeCards (a:b:c:_) = return (a, b, c)

randomCardSelection :: [Card] -> IO Card
randomCardSelection = sample . randomElement

showHand :: [Card] -> IO ()
showHand cs = do
  let sorted = sortBy (comparing suit <> comparing rank) cs
  putStrLn "Your hand:"
  putStrLn . unwords $ map show sorted

promptCard :: [Card] -> IO Card
promptCard cs = do
  showHand cs
  putStr "Enter a card to play: " >> hFlush stdout
  input <- getLine
  let parseError = putStrLn "Not a card!" >> promptCard cs
  maybe parseError return (readCard input)

promptThreeCards :: [Card] -> IO (Card, Card, Card)
promptThreeCards cs = do
  showHand cs
  putStr "Enter three cards to pass: " >> hFlush stdout
  inputs <- words <$> getLine
  case map readCard inputs of
    [Just a, Just b, Just c] -> return (a, b, c)
    _ -> do
      putStrLn "Invalid input..."
      promptThreeCards cs

printMoveInfraction :: MoveInfraction -> IO ()
printMoveInfraction mi = putStrLn $ message mi
  where message CardNotInHand = "That card isn't in your hand!"
        message MustPlayLeadSuit = "You have a card of the lead suit, so you must play it"
        message HeartsNotBroken = "You can't lead hearts until hearts are broken!"
        message NoPointsFirstTrick = "You can't play pounts on the first trick of the round!"

main :: IO ()
main = do
  let randomPlayer = PlayerIO {
    getPassSelections = firstThreeCards,
    getSelectedCard = randomCardSelection,
    receiveFeedback = const $ return (),
    showPreRound = const $ return (),
    showRoundState = const $ return (),
    showPostGame = const $ return ()
  }
  let humanPlayer = PlayerIO {
    getPassSelections = promptThreeCards,
    getSelectedCard = promptCard,
    receiveFeedback = printMoveInfraction,
    showPreRound = printPreRound,
    showRoundState = printRoundState,
    showPostGame = printPostGame
  }
  let piomap = M.fromList
        [ (N, humanPlayer)
        , (E, randomPlayer)
        , (S, randomPlayer)
        , (W, randomPlayer)
        ]
  let gio = GameIO {
    playerIO = piomap
  }
  playGame gio