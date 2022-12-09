{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Condition where

import Control.Lens (at, ix, preview, to, view, _Just, Ixed)
import Count
import Data.Maybe (listToMaybe)
import Data.Text.Lazy (Text, pack)
import Formatting
import Formatting.ShortFormatters (sh)
import Game.Game
import Game.Player
import Location (inventory)

data Condition l r phase play val where
  Num :: Cnt Int -> Condition l r phase play (Cnt Int)
  Bool :: Bool -> Condition l r phase play Bool
  Has :: (Eq r, Show r) => l -> r -> (Cnt Int) -> Condition l r phase play Bool
  HasAtLeast :: (Eq r, Show r) => l -> r -> (Cnt Int) -> Condition l r phase play Bool
  PhaseIsIn :: (Eq phase, Show phase) => [phase] -> Condition l r phase play Bool
  -- PlayIsIn :: [play] -> Condition l r phase play Bool -- cannot fire for Plays, only triggers
  -- TransferIsIn :: [Transfer o u r] -> Condition l r phase play Bool -- cannot fire or Plays, only triggers

  ObservePlayer :: String -> Player -> (Player -> Game l r phase -> Cnt Int) -> Condition l r phase play (Cnt Int)
  ObservePlayerIf :: String -> Player -> (Player -> Game l r phase -> Bool) -> Condition l r phase play Bool
  -- ObserveLocation :: String -> (l -> Game l r phase -> r)  -> Condition l r phase play  val
  ObserveResource :: String -> r -> (r -> Game l r phase -> Cnt Int) -> Condition l r phase play (Cnt Int)
  ObserveResourceIf :: String -> r -> (r -> Game l r phase -> Bool) -> Condition l r phase play Bool
  And :: (Condition l r phase play Bool) -> (Condition l r phase play Bool) -> Condition l r phase play Bool
  Or :: (Condition l r phase play Bool) -> (Condition l r phase play Bool) -> Condition l r phase play Bool
  Not :: (Condition l r phase play Bool) -> Condition l r phase play Bool
  IfThenElse :: Condition l r phase play Bool -> Condition l r phase play val -> Condition l r phase play val -> Condition l r phase play val
  Plus :: (Condition l r phase play (Cnt Int)) -> Condition l r phase play (Cnt Int) -> Condition l r phase play (Cnt Int)
  Minus :: (Condition l r phase play (Cnt Int)) -> Condition l r phase play (Cnt Int) -> Condition l r phase play (Cnt Int)
  Times :: (Condition l r phase play (Cnt Int)) -> Condition l r phase play (Cnt Int) -> Condition l r phase play (Cnt Int)
  GTc :: (Condition l r phase play (Cnt Int)) -> Condition l r phase play (Cnt Int) -> Condition l r phase play Bool
  LTc :: (Condition l r phase play (Cnt Int)) -> Condition l r phase play (Cnt Int) -> Condition l r phase play Bool
  Eq :: Condition l r phase play (Cnt Int) -> Condition l r phase play (Cnt Int) -> Condition l r phase play Bool

ppCondition :: (Show l, Show r) => Condition l r phase play val -> Text
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

evalCondition :: (Ord l, Ord r) => Condition l r phase play val -> Game l r phase -> val
evalCondition (Num i) _ = i
evalCondition (Bool b) _ = b
evalCondition (Has l r cnt) g = preview (#objects . #locations . ix l . to inventory . ix r) g == Just cnt
evalCondition (HasAtLeast l r cnt) g = preview (#objects . #locations . ix l . to inventory . ix r) g >= Just cnt
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
