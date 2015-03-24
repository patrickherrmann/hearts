import Cards
import Hearts

main = do
  deck <- shuffledDeck
  mapM_ (putStrLn . shorthand) deck