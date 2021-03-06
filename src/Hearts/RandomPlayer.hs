module Hearts.RandomPlayer
  ( randomPlayer
  ) where

import Cards
import Hearts
import Data.Random
import Control.Monad

firstThreeCards :: [Card] -> IO [Card]
firstThreeCards = return . take 3

randomCardSelection :: RoundStateView -> IO Card
randomCardSelection = sample . randomElement . handView

randomPlayer :: PlayerIO
randomPlayer = PlayerIO {
  choosePassSelections = firstThreeCards,
  chooseCard = randomCardSelection,
  showMoveInfraction = void . return,
  showGameState = void . return,
  showRoundState = void . return,
  showWinners = void . return
}