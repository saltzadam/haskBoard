{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use uncurry" #-}

module Main where

import Brick (customMainWithDefaultVty)
import Brick.BChan (newBChan)
import Brick.Game.Tui (TUIMode (..), TUIState (..))
import Control.Concurrent.Async (withAsync)
import Control.Lens ((^.))
import Control.Monad (void)
import qualified Data.Map as M
import qualified Data.Set as S
import Game.Player (Player (..))
import Game.View (viewGameStateAs')
import Interface.Agent (brickAgent, randomAgent, runAgentIO)
import Interface.Controller (PlayerInterface (..), buildInterface)
import Interface.Server (server)
import NoMerci
import Run (runGameSeparateChannels)
import Tui (app)

withWorker :: IO a -> IO a -> IO a
withWorker outer inner = withAsync outer $ const inner

main :: IO ()
main = do
  let gs = fst (noMerci 3)
  let gr = snd (noMerci 3)
  let players = S.toList (gs ^. #players)
  interface <- buildInterface players
  let channels = fmap (\(PlayerInterface fromChan toChan) -> (fromChan, toChan)) (interface ^. #playerInterfaces)

  withWorker
    ( void $
        runGameSeparateChannels
          interface
          gs
          gr
    )
    (server interface)

-- -- need to start writing to channels before reading them
-- main :: IO ()
-- main = do
--   let gs = fst (noMerci 3)
--   let gr = snd (noMerci 3)
--   let players = S.toList (gs ^. #players)
--   interface <- buildInterface players
--   let channels = fmap (\(PlayerInterface fromChan toChan) -> (fromChan, toChan)) (interface ^. #playerInterfaces)
--   gameToBrickBChan <- newBChan 100
--   brickToGameBChan <- newBChan 100
--   let playerAgent = brickAgent (fst $ channels M.! Player 1) gameToBrickBChan (snd $ channels M.! Player 1) brickToGameBChan
--   let ai1 = uncurry randomAgent (channels M.! (Player 2))
--   let ai2 = uncurry randomAgent (channels M.! (Player 3))
--
--   let gsv = viewGameStateAs' gs (Player 1)
--   let initTUI = TUIState gsv (Player 1) ShowState [] brickToGameBChan Nothing True []
--
--   withWorker (runAgentIO playerAgent)
--     $ withWorker (runAgentIO ai1)
--     $ withWorker (runAgentIO ai2)
--     $ withWorker
--       ( void $
--           runGameSeparateChannels
--             interface
--             gs
--             gr
--       )
--     $ (void $ customMainWithDefaultVty (Just gameToBrickBChan) app initTUI)
