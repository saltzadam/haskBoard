{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
    {-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
module Game
    where

import Count
-- import Data.Finitary
import Control.Lens ( makeFields, at, view, to, mapped, preview )
import Control.Lens.Prism
import Location
import GHC.Generics (Generic)
import Text.Show.Functions()
import Data.Text.Lazy (Text, pack)
import Formatting
import Formatting.ShortFormatters (d, sh)

data Player = Player {id :: Int, name :: String} deriving (Eq, Ord, Show, Generic)

data Game onames unames resources phases = Game {
    players :: [Player],
    locations :: GameObjects onames unames resources,
    phase :: phases
                                                           } deriving (Generic)

makeFields ''Game
-- Data representations of a Transfer
-- Key interpreter is to transfer function
-- stuff source target
data Transfer resource loc loc' = Transfer resource loc loc' deriving (Eq, Ord, Show, Generic)

mkTransfer :: Ord r => Transfer r (Location t r) (Location t' r) -> (Location t r, Location t' r, Maybe r)
mkTransfer (Transfer r l l') = transfer r l l'
--
-- tons of redundancy

data Condition o u r p val where
    Num :: Cnt Int -> Condition o u r p (Cnt Int)
    Bool :: Bool -> Condition o u r p Bool
    Has :: (Eq r, Show lname, Show r) => Either o u -> r -> (Cnt Int) -> Condition o u r p  Bool
    HasAtLeast :: (Eq r, Show lname, Show r) => Either o u -> r -> (Cnt Int) -> Condition o u r p  Bool
    PhaseIsIn :: [p] -> Condition o u r p Bool

    ObservePlayer :: String -> Player -> (Player -> Game o u r p -> Cnt Int) -> Condition o u r p (Cnt Int)
    ObservePlayerIf :: String -> Player -> (Player -> Game o u r p -> Bool) -> Condition o u r p Bool
    -- ObserveLocation :: String -> (l -> Game o u r -> r)  -> Condition o u r p  val
    ObserveResource :: String -> r -> (r -> Game o u r p -> Cnt Int) -> Condition o u r p (Cnt Int)
    ObserveResourceIf :: String -> r -> (r -> Game o u r p -> Bool) -> Condition o u r p Bool

    And :: (Condition o u r p  Bool) -> (Condition o u r p  Bool) -> Condition o u r p  Bool
    Or :: (Condition o u r p  Bool) -> (Condition o u r p  Bool) -> Condition o u r p Bool
    Not :: (Condition o u r p  Bool) -> Condition o u r p  Bool
    IfThenElse :: Condition o u r p  Bool -> Condition o u r p  val -> Condition o u r p  val -> Condition o u r p val
    Plus ::  (Condition o u r p  (Cnt Int)) -> Condition o u r p  (Cnt Int) -> Condition o u r p  (Cnt Int)
    Minus :: (Condition o u r p  (Cnt Int)) -> Condition o u r p  (Cnt Int) -> Condition o u r p  (Cnt Int)
    Times :: (Condition o u r p  (Cnt Int)) -> Condition o u r p  (Cnt Int) -> Condition o u r p  (Cnt Int)
    GTc ::  (Condition o u r p  (Cnt Int)) -> Condition o u r p  (Cnt Int) -> Condition o u r p  Bool
    LTc ::  (Condition o u r p  (Cnt Int)) -> Condition o u r p  (Cnt Int) -> Condition o u r p  Bool
    Eq ::  Condition o u r p  (Cnt Int) -> Condition o u r p  (Cnt Int) -> Condition o u r p  Bool

ppCondition :: (Show r, Show o, Show u, Show p) => Condition o u r p val -> Text
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

evalCondition :: (Ord o, Ord u, Ord r, Eq p) => Condition o u r p val -> Game o u r p -> val
evalCondition (Num i) _ = i
evalCondition (Bool b) _ = b
evalCondition (Has (Left oloc) r cnt) g =  preview (#locations . #decks . at oloc . _Just . lensDeck . to (countF r)) g  == Just cnt
evalCondition (Has (Right uloc) r cnt) g =  preview (#locations . #piles . at uloc . _Just . lensPile . at r . _Just) g  == Just cnt
evalCondition (HasAtLeast (Left oloc) r cnt) g =  preview (#locations . #decks . at oloc . _Just . lensDeck . to (countF r)) g  >= Just cnt
evalCondition (HasAtLeast (Right uloc) r cnt) g =  preview (#locations . #piles . at uloc . _Just . lensPile . at r . _Just) g  >= Just cnt
evalCondition (PhaseIsIn ps) g = view #phase g `elem` ps
evalCondition (ObservePlayer _ pl f) g = f pl g
evalCondition (ObservePlayerIf _ pl f) g = f pl g
evalCondition (ObserveResource _ res f) g = f res g
evalCondition (ObserveResourceIf _ res f) g = f res g
evalCondition (And c c') g = evalCondition c g && evalCondition c' g
evalCondition (Or c c') g = evalCondition c g ||  evalCondition c' g
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
--  Plays may have multiple stages. E.g. draw a card, then choose and discard a card.
--  So like state -> [m Transfer] ?
--
--  Plays may have follow-ups from game logic. E.g. discard to hand size or "visit by
--  a noble." These should be part of the game logic, not the data of an individual play.


-- Don't want to actually use state -> [Transfer] -- only has one interpretation
-- Use DSL to write these.

-- Not sure about multiple stages. 


{- 
Games also have control flow. The game rules will dictate everything that happens until a player
needs to make a choice. This choice is called a Play. The game rules determine
which Plays are legal. The game can enumerate the legal plays from any position.

-}
