{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use uncurry" #-}

module Main where

import Control.Monad (when)
import Data.List (elemIndex)
import Data.Maybe (fromJust, fromMaybe)
import NoMerci (noMerci)
import Interface.Protocol (RewardConfig (..))
import Run.Game (RunMode (..), runGame)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Read (readMaybe)
import Tui (app)

main :: IO ()
main = do
  args <- getArgs
  let argPairs = zip args (drop 1 args)
      numPlayers = fromMaybe 3 $ lookup "--players" argPairs >>= readMaybe
  when (numPlayers < 3 || numPlayers > 5) $ do
    putStrLn "Error: --players must be between 3 and 5"
    exitFailure
  let run = runGame noMerci (Just app) "logs/nomerci.log" "logs/nomerci.json" numPlayers
  case () of
    _ | "--stdio"     `elem` args -> run (Stdio ZeroSum)
      | "--collect"   `elem` args -> run Collect
      | "--ws-agents" `elem` args ->
          let checkpoint = args !! succ (fromJust (elemIndex "--ws-agents" args))
              humanN     = maybe 0 read (lookup "--human-player" argPairs)
          in run (WSAgents checkpoint humanN)
      | otherwise -> do
          putStrLn "Usage: NoMerci --ws-agents <checkpoint> [--human-player N] [--players N]"
          putStrLn "       NoMerci --stdio [--players N]"
          putStrLn "       NoMerci --collect [--players N]"
          putStrLn "  --players N: number of players, 3-5 (default 3)"
          exitFailure
