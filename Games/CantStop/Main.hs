module Main where
import GameE (action)
import CantStop (initGameData, gameRules)

a = action (initGameData 3) gameRules

main :: IO ()
main = do
    gd <- a
    putStrLn "ok"
