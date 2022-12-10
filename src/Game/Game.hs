{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Game where

import Control.Lens (Ixed (..), at, makeFields, preview, reviews, set, view, (^.), _Just)
import Count
import GHC.Generics (Generic)
import Game.Player (Player)
import Location (Counter (..), GameObjects)
import System.Random.Stateful (StdGen, uniformR)

data Game lnames resources phase = Game
  { players :: [Player],
    objects :: GameObjects lnames resources,
    phaseStack :: [phase], -- provisional
    activePlayer :: Maybe Player,
    randGen :: StdGen
  }
  deriving (Generic)

makeFields ''Game

roll :: Ord l => l -> Game l r p -> (Game l r p, Maybe (Cnt Int))
roll l game = case preview (#objects . #counters . ix l) game of
  Nothing -> (game, Nothing)
  Just (Counter _ (bl, bu)) ->
    let (a', g') = uniformR (bl, bu) (game ^. #randGen)
        newC = Counter (Just a') (bl, bu)
     in ( set (#objects . #counters . ix l) newC
            . set #randGen g'
            $ game,
          Just a'
        )
