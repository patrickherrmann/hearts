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
    putStrLn "\nPot:"
    showPMap (pot rs) shorthand

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
  let pio = PlayerIO {
    getPassSelections = firstThreeCards,
    getSelectedCard = randomCardSelection
  }
  let piomap = M.fromList . zip players $ repeat pio
  let gio = GameIO {
    showPreRound = printPreRound,
    showRoundState = printRoundState,
    showPostGame = printPostGame,
    playerIO = piomap
  }
  playGame gio