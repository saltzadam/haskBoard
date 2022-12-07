{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Formatting
import Formatting.ShortFormatters (d, sh)
import GHC.Generics (Generic)
import Location
import Text.Show.Functions ()
import Data.Maybe (listToMaybe)

data Player = Player {id :: Int, name :: String} deriving (Eq, Ord, Show, Generic)

data Game onames unames snames resources phase = Game
  { players :: [Player],
    locations :: GameObjects onames unames snames resources,
    phaseStack :: [phase], -- provisional
    activePlayer :: Maybe Player
  }
  deriving (Generic)

makeFields ''Game

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

data Condition o u s r phase play val where
  Num :: Cnt Int -> Condition o u s r phase play (Cnt Int)
  Bool :: Bool -> Condition o u s r phase play Bool
  Has :: (Eq r, Show lname, Show r) => Either o u -> r -> (Cnt Int) -> Condition o u s r phase play Bool
  HasAtLeast :: (Eq r, Show lname, Show r) => Either o u -> r -> (Cnt Int) -> Condition o u s r phase play Bool
  PhaseIsIn :: (Eq phase, Show phase) => [phase] -> Condition o u s r phase play Bool
  -- PlayIsIn :: [play] -> Condition o u s r phase play Bool -- cannot fire for Plays, only triggers
  -- TransferIsIn :: [Transfer o u r] -> Condition o u s r phase play Bool -- cannot fire or Plays, only triggers

  ObservePlayer :: String -> Player -> (Player -> Game o u s r phase -> Cnt Int) -> Condition o u s r phase play (Cnt Int)
  ObservePlayerIf :: String -> Player -> (Player -> Game o u s r phase -> Bool) -> Condition o u s r phase play Bool
  -- ObserveLocation :: String -> (l -> Game o u r -> r)  -> Condition o u s r phase play  val
  ObserveResource :: String -> r -> (r -> Game o u s r phase -> Cnt Int) -> Condition o u s r phase play (Cnt Int)
  ObserveResourceIf :: String -> r -> (r -> Game o u s r phase -> Bool) -> Condition o u s r phase play Bool
  And :: (Condition o u s r phase play Bool) -> (Condition o u s r phase play Bool) -> Condition o u s r phase play Bool
  Or :: (Condition o u s r phase play Bool) -> (Condition o u s r phase play Bool) -> Condition o u s r phase play Bool
  Not :: (Condition o u s r phase play Bool) -> Condition o u s r phase play Bool
  IfThenElse :: Condition o u s r phase play Bool -> Condition o u s r phase play val -> Condition o u s r phase play val -> Condition o u s r phase play val
  Plus :: (Condition o u s r phase play (Cnt Int)) -> Condition o u s r phase play (Cnt Int) -> Condition o u s r phase play (Cnt Int)
  Minus :: (Condition o u s r phase play (Cnt Int)) -> Condition o u s r phase play (Cnt Int) -> Condition o u s r phase play (Cnt Int)
  Times :: (Condition o u s r phase play (Cnt Int)) -> Condition o u s r phase play (Cnt Int) -> Condition o u s r phase play (Cnt Int)
  GTc :: (Condition o u s r phase play (Cnt Int)) -> Condition o u s r phase play (Cnt Int) -> Condition o u s r phase play Bool
  LTc :: (Condition o u s r phase play (Cnt Int)) -> Condition o u s r phase play (Cnt Int) -> Condition o u s r phase play Bool
  Eq :: Condition o u s r phase play (Cnt Int) -> Condition o u s r phase play (Cnt Int) -> Condition o u s r phase play Bool

ppCondition :: (Show r, Show o, Show u, Show play, Show s, Show phase) => Condition o u s r phase play val -> Text
ppCondition (Num i) = pack (show i)
ppCondition (Bool b) = pack (show b)
ppCondition (Has l f i) = format (sh %+ "has" %+ sh %+ sh) l i f
ppCondition (HasAtLeast l f i) = format (sh %+ "has at least" %+ sh %+ sh) l i f
ppCondition (PhaseIsIn ps) = format ("Game phase is one of:" %+ sh) ps
ppCondition (ObservePlayer name p _) = pack (name ++ show p)
ppCondition (ObservePlayerIf name p _) = pack (name ++ show p)
ppCondition (ObserveResource name r _) = pack (name ++ show r)
ppCondition (ObserveResourceIf name r _) = pack (name ++ show r)
-- why switch here lol
ppCondition (And c c') = ppCondition c <> " and  " <> ppCondition c'
ppCondition (Or c c') = ppCondition c <> " or " <> ppCondition c'
ppCondition (Not c) = "not " <> ppCondition c
ppCondition (IfThenElse c c' c'') = "If " <> ppCondition c <> " then " <> ppCondition c' <> ". Otherwise " <> ppCondition c''
ppCondition (Plus c c') = ppCondition c <> " plus " <> ppCondition c'
ppCondition (Minus c c') = ppCondition c <> " minus " <> ppCondition c'
ppCondition (Times c c') = ppCondition c <> " times " <> ppCondition c'
ppCondition (Eq c c') = ppCondition c <> " equals " <> ppCondition c'
ppCondition (GTc c c') = ppCondition c <> " greater than " <> ppCondition c'
ppCondition (LTc c c') = ppCondition c <> " less than " <> ppCondition c'

evalCondition :: (Ord o, Ord u, Ord r) => Condition o u s r phase play val -> Game o u s r phase -> val
evalCondition (Num i) _ = i
evalCondition (Bool b) _ = b
evalCondition (Has (Left oloc) r cnt) g = preview (#locations . #decks . at oloc . _Just . lensDeck . to (foldl (\acc a -> if a == r then acc + 1 else acc) 0)) g == Just cnt
evalCondition (Has (Right uloc) r cnt) g = preview (#locations . #piles . at uloc . _Just . lensPile . at r . _Just) g == Just cnt
evalCondition (HasAtLeast (Left oloc) r cnt) g = preview (#locations . #decks . at oloc . _Just . lensDeck . to (foldl (\acc a -> if a == r then acc + 1 else acc) 0)) g >= Just cnt
evalCondition (HasAtLeast (Right uloc) r cnt) g = preview (#locations . #piles . at uloc . _Just . lensPile . at r . _Just) g >= Just cnt
evalCondition (PhaseIsIn ps) g = case listToMaybe (view #phaseStack g) of
                                   Nothing -> False
                                   Just i -> i `elem` ps
evalCondition (ObservePlayer _ pl f) g = f pl g
evalCondition (ObservePlayerIf _ pl f) g = f pl g
evalCondition (ObserveResource _ res f) g = f res g
evalCondition (ObserveResourceIf _ res f) g = f res g
evalCondition (And c c') g = evalCondition c g && evalCondition c' g
evalCondition (Or c c') g = evalCondition c g || evalCondition c' g
evalCondition (Not c) g = not (evalCondition c g)
evalCondition (IfThenElse c c' c'') g = if evalCondition c g then evalCondition c' g else evalCondition c'' g
evalCondition (Plus c c') g = evalCondition c g + evalCondition c' g
evalCondition (Minus c c') g = evalCondition c g - evalCondition c' g
evalCondition (Times c c') g = evalCondition c g * evalCondition c' g
evalCondition (Eq c c') g = evalCondition c g == evalCondition c' g
evalCondition (GTc c c') g = evalCondition c g > evalCondition c' g
evalCondition (LTc c c') g = evalCondition c g < evalCondition c' g

-- validateCondition :: EvalCondition r l Bool -- dunno
-- validateCondition = undefined

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

