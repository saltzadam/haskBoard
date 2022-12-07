{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Game.Condition where

import Data.Text.Lazy (Text, pack)
import Formatting
import Formatting.ShortFormatters ( sh)
import Control.Lens (preview, at, _Just, to, view)
import Data.Maybe (listToMaybe)

import Count
import Location
import Game.Player
import Game.Game
data Condition o u s r phase play val where
  Num :: Cnt Int -> Condition o u s r phase play (Cnt Int)
  Bool :: Bool -> Condition o u s r phase play Bool
  Has :: (Eq r, Show r) => Either o u -> r -> (Cnt Int) -> Condition o u s r phase play Bool
  HasAtLeast :: (Eq r,  Show r) => Either o u -> r -> (Cnt Int) -> Condition o u s r phase play Bool
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


