module Main where
import CantStop (initGameState, moreInterestingGameState, csRunPlay)
import Brick (defaultMain)
import Tui (app, drawBoard)
import Brick.Main (simpleMain)
import GameE (Game(Game))
import Brick.BChan (newBChan)


main :: IO ()
main = do
    putStrLn "go"
    s <- moreInterestingGameState
    putStrLn "go"
    _ <- defaultMain  app (Game s csRunPlay)
    pure ()


-- streamMain :: IO ()
-- streamMain = do
--     chan <- newBChan 100
--     forkIO $ forever $ do
--         writeBChan 

--     undefined
