import Cards
import Hearts
import Text.Printf
import qualified Data.Map as M
import Data.Random

showPMap :: PMap a -> (a -> String) -> IO ()
showPMap pmap toS = mapM_ (putStrLn . showP) $ M.assocs pmap
    where showP (p, a) = show p ++ ": " ++ toS a

printPreRound :: GameState -> IO ()
printPreRound gs = do
    putStrLn "\nScores: -------------"
    showPMap (scores gs) show

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
    getPassSelections = firstThreeCards,
    getSelectedCard = randomCardSelection,
    receiveFeedback = putStrLn,
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