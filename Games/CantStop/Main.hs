module Main where
import CantStop (initGameState, moreInterestingGameState, csRunPlay, csVisibility, runCSTurns)
import Brick (defaultMain)
import Tui (app, drawBoardView)
import Brick.Main (simpleMain)
import GameE (Game(Game), action)
import Brick.BChan (newBChan)
import View (viewGameAs', viewGameStateAs')
import Game.Player (Player(..))
import Visibility (allVisible)


main :: IO ()
main = do
    putStrLn "go"
    s <- runCSTurns -- action (initGameState 3) csRunPlay csVisibility
    _ <- defaultMain app (viewGameStateAs' s allVisible (Player 1))
    putStrLn "go"
    pure ()


-- streamMain :: IO ()
-- streamMain = do
--     chan <- newBChan 100
--     forkIO $ forever $ do
--         writeBChan 

--     undefined
