import Cards
import Hearts

main = mapM_ (putStrLn . shorthand) $ fullDeck