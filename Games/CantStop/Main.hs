module Main where
import CantStop (initGameState, moreInterestingGameState, csRunPlay, csVisibility)
import Brick (defaultMain)
import Tui (app, drawBoard)
import Brick.Main (simpleMain)
import GameE (Game(Game), action)
import Brick.BChan (newBChan)


main :: IO ()
main = do
    putStrLn "go"
    s <- action (initGameState 3) csRunPlay csVisibility
    -- putStrLn "go"
    -- _ <- defaultMain  app (Game s csRunPlay csVisibility)
    pure ()


-- streamMain :: IO ()
-- streamMain = do
--     chan <- newBChan 100
--     forkIO $ forever $ do
--         writeBChan 

--     undefined
