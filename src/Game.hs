{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Game where

-- import Data.Finitary
import Control.Lens (at, makeFields, preview, to, view)
import Control.Lens.Prism
import Count
import Data.Text.Lazy (Text, pack)
import GHC.Generics (Generic)
import Location
import Text.Show.Functions ()
import Data.Maybe (listToMaybe)
import Game.Player
import Game.Condition
import Game.Game

-- Data representations of a Transfer
-- Key interpreter is to transfer function
-- stuff source target
data Transfer resource loc loc' = Transfer resource loc loc' deriving (Eq, Ord, Show, Generic)

mkTransfer' :: Ord r => Transfer r (Location t r) (Location t' r) -> (Location t r, Location t' r, Maybe r)
mkTransfer' (Transfer r l l') = transfer r l l'

-- mkTransfer :: Ord r => Transfer r (Location t r) (Location t' r) -> Game o u r ph -> Game o u r ph
-- mkTransfer (Transfer r (Location  r) ) g = 
    

--
-- tons of redundancy

-- What is a Play?
--  Computes Transfers from the game state. Like buying a card costs some gems, but
--  need the rest of the state to actually compute the Transfer.
--  So like state -> [Transfer]
--
--  Plays may have follow-ups from game logic. E.g. discard to hand size or "visit by
--  a noble." These should be part of the game logic, not the data of an individual play.

-- Before working out Play, need to think some about control flow.
-- Games have Phases. Any sequetial game has phases
-- data SequentialPhases = PreTurn Player | MainPhase Player | PostTurn Player
-- Any other time a player needs to make a choice, that's a phase.
-- data SequentialPhases aux = PreTurn Player | MainPhase Player | PostTurn Player | CustomPhase aux
-- Really need to just leave this up to game -- will have common patterns, but real danger of overengineering.
--
-- Control flow describes:
--   - what happens when you enter a phase
--   - how a Play moves from phase to phase
-- But need some memory here.
--
-- E.g. a player plays "draw a card, then discard a card." The Card is Transfered to Hand Player.
-- Phase moves from MainPhase Player to AuxPhaseDiscard Player. The player now has to choose a play.
-- (Is that a property of phases? Or?)
-- The only play legal in AuxPhaseDiscard is "discard a card", so the player chooses a card. That card is
-- Transfered to DiscardPile Player.
-- Now phase should move back to MainPhase Player.
--
-- But if in PostTurn Player, another player has to discard to hand size, then AuxPhaseDiscard has to return
-- control to PostTurn Player.
--
--

-- When a player choose a play, put it as the root of a tree
-- Next level is a sequence of transfers, phase changes, and plays.
-- Each of those may trigger further events.
-- So we get a tree. In most games it's traversed breadth-first.

-- When the tree has been completely traversed, the play is resolved.

-- Games probably shouldn't export this -- use smart constructor
-- e.g. takeTokens color player = Play ..

-- For now: just Plays that do transfers!

-- data GameAction o u s r phase play = ChangePhase phase | MakeTransfer (Transfer o u r) | MakePlay (Play o u s r phase play)

data Play o u s r phase play = Play
  { legalCondition :: Condition o u s r phase play Bool,
    makeMoves :: Game o u s r phase -> [Transfer o u r],
    owner :: Maybe Player
  }

