{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
module Game where

-- import Data.Finitary
import GHC.Generics (Generic)
import Game.Player
import Location
import Text.Show.Functions ()
import Count
import Control.Monad.Random (StdGen)
import Control.Lens hiding (Empty)
import Prettyprinter (Pretty (..), viaShow, (<+>))
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT, reader)

-- Data representations of a Transfer
-- Key interpreter is to transfer function
-- stuff source target
data Transfer resource lname = Transfer resource lname lname deriving (Eq, Ord, Show, Generic)

mkTransfer' :: (Ord name, Ord r) => Transfer r name -> Locations name r -> (Locations name r, Maybe r)
mkTransfer' (Transfer r l l') = transfer r l l'

data GameAction l r phaseName = ChangePhase phaseName
                              | MakeTransfer (Transfer l r)
                              | IncrementCounter l
                              | DecrementCounter l
                              | SetCounter l (Cnt Int)
                              | RollCounter l
                                  -- | MakePlay (Play o u s r phase play)
                              deriving (Eq, Ord, Show, Generic)

data PhaseControl = One Player
                | Sequential [Player]
                | Simultaneous [Player]
                | None deriving (Eq, Ord, Show, Generic)


data C2  l r ph a where
    -- Loc :: l -> C2 l r ph l
    -- Res :: r -> C2 l r ph r
    -- Num :: Cnt Int -> C2 l r ph (Cnt Int)
    -- PhaseLit :: ph -> C2 l r ph ph
    -- BoolLit :: Bool -> C2 l r ph Bool
    -- Action :: GameAction l r ph -> C2 l r ph (GameAction l r ph)
    Lit :: a -> C2 l r ph a

    Has2 :: (Ord l, Ord r) => l -> r -> C2 l r ph (Cnt Int)
    CounterVal2 :: Ord l => l -> C2 l r ph (Cnt Int)

    Empty :: C2 l r ph [a]
    Cons :: (Show a, Eq a) => C2 l r ph a -> C2 l r ph [a] -> C2 l r ph [a]
    In :: (Show a,  Eq a) => C2 l r ph a -> C2 l r ph [a] -> C2 l r ph Bool

    If :: C2 l r ph (Bool -> a -> a -> a)

    Pair :: C2 l r ph (a -> b -> (a,b))
    Fst :: C2 l r ph ((a,b) -> a)
    Snd :: C2 l r ph ((a,b) -> b)

    Apply :: C2 l r ph (a -> b) -> C2 l r ph a -> C2 l r ph b
    Lam :: (C2 l r ph a -> C2 l r ph b) -> C2 l r ph (a -> b)

    Plus :: C2 l r ph (Cnt Int) -> C2 l r ph (Cnt Int) -> C2 l r ph (Cnt Int)
    Minus :: C2 l r ph (Cnt Int) -> C2 l r ph (Cnt Int) -> C2 l r ph (Cnt Int)
    Times :: C2 l r ph (Cnt Int) -> C2 l r ph (Cnt Int) -> C2 l r ph (Cnt Int)
    Abs :: C2 l r ph (Cnt Int) -> C2 l r ph (Cnt Int)
    Sign :: C2 l r ph (Cnt Int) -> C2 l r ph (Cnt Int)

    Or :: C2 l r ph Bool -> C2 l r ph Bool -> C2 l r ph Bool
    And :: C2 l r ph Bool -> C2 l r ph Bool -> C2 l r ph Bool
    Not :: C2 l r ph Bool -> C2 l r ph Bool

    CEq :: Eq a => C2 l r ph a -> C2 l r ph a -> C2 l r ph Bool
    CGTEq :: Ord a => C2 l r ph a -> C2 l r ph a -> C2 l r ph Bool

data Game l r phaseName playName = Game
  { players :: [Player],
    objects :: GameObjects l r,
    phaseStack :: [phaseName], -- provisional
    activePlayer :: Maybe Player,
    plays :: Maybe Player -> playName -> C2 l r phaseName [GameAction l r phaseName],
    randGen :: StdGen
  }
  deriving (Generic)

makeFields ''Game

evalC ::  C2 l r ph a -> ReaderT (Game l r ph pl) Identity a
-- evalC (Loc l) = return l
-- evalC (Res r) = return  r
-- evalC (Num n) = return n
-- evalC (PhaseLit ph) = return ph
-- evalC (BoolLit b) = return b
-- evalC (Action a) = return a
evalC (Lit a) = return a
evalC (Has2 l r) = view (#objects . #locations . at l . non Dummy . to inventory . at r . non 0) <$> ask
evalC (CounterVal2 l) = maybe 0 (view #val) . preview (#objects . #counters . ix l) <$> ask

evalC If = return $ \c t f -> if c then t else f

evalC Pair = return (,)
evalC Fst = return fst
evalC Snd = return snd

evalC Empty = return []
evalC (Cons l lv) = (:) <$> evalC l <*> evalC lv
evalC (In l lv) = elem <$> evalC l <*> evalC lv

evalC (Apply f a) = evalC f <*> evalC a
evalC (Lam f) = reader $ (runIdentity .) . flip (runReaderT . evalC . f . Lit)
evalC (Plus a b) = (+) <$> evalC a <*> evalC b
evalC (Minus a b) = (-) <$> evalC a <*> evalC b
evalC (Times a b) = (*) <$> evalC a <*> evalC b
evalC (Abs a) = fmap abs (evalC a)
evalC (Sign a) = fmap signum (evalC a)
evalC (Or a b) = (||) <$> evalC a <*> evalC b
evalC (And a b) = (&&) <$> evalC a <*> evalC b
evalC (Not a) = fmap not (evalC a)
evalC (CEq a b) = (==) <$> evalC a <*> evalC b
evalC (CGTEq a b) = (>=) <$> evalC a <*> evalC b


instance (Show l, Show r, Show ph, Show a) => Pretty (C2 l r ph a) where
    pretty (Lit a) = viaShow a
    pretty (Has2 l r) = "the amount of" <+> viaShow r <+> "at" <+> viaShow l
    pretty (CounterVal2 l) = "the value of" <+> viaShow l
    pretty (Cons x xs@(Cons y ys)) = pretty x <> "," <> pretty xs
    pretty (Cons x Empty) = pretty x <> "]"
    pretty Empty = ""

    pretty (If `Apply` b `Apply` t `Apply` f) = "If" <+> pretty b <+> "then" <+> pretty t <+> "else" <+> pretty f

    -- pretty (I
    -- pretty (Cons v vs) = encloseSep "[" "]" "," vs





data Phase phaseName l r playName = Phase {
    name :: phaseName,
    enterAction :: C2 l r phaseName [GameAction l r phaseName],
    exitAction :: C2 l r phaseName [GameAction l r phaseName],
    possiblePlays :: [playName],
    legal :: C2 l r phaseName (playName -> Bool),
    control :: PhaseControl
                                      }

true :: C2 l r ph Bool
true = Lit True
false :: C2 l r ph Bool
false = Lit False

instance Num (C2 l r ph (Cnt Int)) where
    (+) a b = a `Plus` b
    (-) a b = a `Minus` b
    (*) a b = a `Times` b
    abs = Abs
    signum = Sign
    fromInteger i = Lit (Cnt (fromInteger i))















-- think of C2 l r a as being part of Reader (Game l r) a
-- so any function Game -> b should be representable as C2 l r b
--      ^ that's the 'game logic' abstracted out from the function itself
-- possibleOptions :: Game -> [Play] =====> C2 l r [Play]
-- legal :: Game -> Play -> bool =====> C2 l r (Play -> Bool)


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

-- For now: just Plays that do transfers or phases!


