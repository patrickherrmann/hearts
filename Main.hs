import Cards
import Hearts

main = mapM_ (\c -> putStrLn (showUnicode c ++ " " ++ shorthand c)) fullDeck