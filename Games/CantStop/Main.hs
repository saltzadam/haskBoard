module Main where
import GameE (action)
import CantStop (initGameState, gameRules)

a = action (initGameState 3) gameRules

main :: IO ()
main = do
    gd <- a
    putStrLn "ok"
