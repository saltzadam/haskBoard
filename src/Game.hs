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
    {-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Game where

-- import Data.Finitary
import GHC.Generics (Generic)
import Game.Player
import Location
import Text.Show.Functions ()
import Control.Monad.Random (StdGen)
import Control.Lens hiding (Empty, Choice)
import Control.Applicative
import Control.Monad.Trans.State
import Data.Monoid (Endo (..))
import Data.Maybe (mapMaybe)
import Data.Foldable (traverse_)
import Count

-- Need to define some types before Game.

-- These are the fundamental actions in a game. All the "verbs" of a game (besides the observations, e.g. "check" and "count") should be phrase in terms of these.
data GameAction l r ph = DoNothing
    | MkTransfer l l r
    | IncrementCounter l
    | DecrementCounter l
    | SetCounter l (Cnt Int)
    | RollCounter l
    | ChangePhase ph
    deriving (Eq, Ord, Show, Generic)


-- Computations within a game which produce `a`.
-- Want a good interace here so that we can evaluate, pretty-print, parse/validate, etc.
-- Don't have that yet.
newtype Condition l r ph pl t tn a = Condition {runCondition :: GameT l r ph pl t tn a}
    deriving (Functor, Applicative, Monad, Generic)

instance Semigroup a => Semigroup (Condition l r ph pl t tn a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (Condition l r ph pl t tn a) where
    mempty = return mempty

-- Compute a condition given a `Game`.
-- Shows redunacy of current definition?
evalCondition ::  Condition l r ph pl t tn a  -> (GameT l r ph pl t tn) a
evalCondition = view #runCondition

-- To make Conditions easier to work with
instance Num a => Num (Condition l r ph pl t tn a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)
    abs = fmap abs
    signum = fmap signum
    fromInteger  = pure . fromInteger



-- A play `pl` is just a choice that a player must make. `Choice` is a set of plays
-- to be presented to a player.
data Choice pl = Choice Player [pl] deriving Generic



-- The flow of a game looks like this: there is some sequence of `GaAeActions` (draw a card, advance the turn counter) until a player must make a `Choice`. Choices produce sequences of actions and additional choices, and so on. Also, 'GameAction` can indirectly produce choices via `Triggers`. For those `Triggers`, it's important to keep track of the parent actions and sources. Putting all of this together, we get a tree. The nodes are `GameNode`s.
--
-- `source` is a kind of shorthand -- just to make sure that triggers do not trigger themselves, for example.
data GameNode l r ph pl = GameNode {
        -- priority :: Int, -- don't need this yet
        node :: Either (Choice pl) (GameAction l r ph),
        source :: Maybe (l,r),
        parents :: [GameNode l r ph pl]
                                   } deriving (Generic)



-- `Triggers` are checked after each action.
-- The main thing is the `condition`. Given an action and a list of sources (immediate source at head)
-- and an action, should the trigger fire? If so, it will produce `GameNodes`.
data Trigger l r ph pl t name = Trigger { condition :: [(l,r)] -> GameAction l r ph -> Condition l r ph pl t name [GameNode l r ph pl], -- should be NE list of sources
                                     name :: name,
                                     source :: (l,r)
                                     -- prioirty :: Int
                                   } deriving (Generic)

runTrigger :: Trigger l r ph pl t name -> [(l,r)] -> GameAction l r ph -> Condition l r ph pl t name [GameNode l r ph pl]
runTrigger = view #condition

instance Show name => Show (Trigger l r ph pl t name) where
    show t = show (t ^. #name)

act :: (Ord l, Ord r) => GameAction l r phaseName -> GameT l r phaseName playName turns triggerName ()
act DoNothing = return ()
act (MkTransfer l l' r) = modifying (#objects . #locations) (transfer r l l')
act (IncrementCounter l) = modifying (#objects . #counters . ix l) increment
act (DecrementCounter l) = modifying (#objects . #counters . ix l) decrement 
act (SetCounter l v) = modifying (#objects . #counters . ix l) (`setCounter` v)
act (RollCounter l) = modifying (#objects . #counters . ix l) rollCounter
act (ChangePhase ph) = undefined -- TODO: while we figure out control flow

-- For now, `Game` is a big record of functions
-- Could be replaced by something more monadic.
-- Define a State type right below.
data Game l r phaseName playName turns triggerName = Game
  { players :: [Player],
    objects :: GameObjects l r,
    runPlay ::  playName -> Condition l r phaseName playName turns triggerName [GameNode l r phaseName playName],
    randGen :: StdGen,
    chooser :: Choice playName -> playName,
    triggers :: [Trigger l r phaseName playName turns triggerName],
    advancePlayer :: Game l r phaseName playName turns triggerName -> Maybe Player,
    activePlayer :: Maybe Player,
    turnNumber :: turns
  }
  deriving (Generic)

type GameT l r ph pl t tn = State (Game l r ph pl t tn)

makeFields ''Game

-- could rewrite GameT in this style?
-- class Monad m => MonadChoice m pl where
--     choose :: Choice pl -> m pl

-- But for now use this, basically ReaderT pattern.
choosePlay :: Choice pl -> (GameT l r ph pl t tn) pl
choosePlay c =  do
        g <- get
        let chooser = view #chooser g
        return (chooser c)

-- Given a `Choice`, create the appropriate Actions and decisions
-- TODO: Triggers should pick up plays as well.
chooseNode ::  Choice pl -> GameT l r ph pl t tn [GameNode l r ph pl]
chooseNode c = do
    pl <- choosePlay c
    playRunner <- use #runPlay
    evalCondition (playRunner pl)

-- Evaluate all the triggers on a particular instance of a `GameAction`.
-- TODO: As above, Triggers should pick up plays/choices as well.
getTriggers :: [(l,r)] -> GameAction l r ph -> GameT l r ph pl t tn [GameNode l r ph pl]
getTriggers sources action = do
    triggers <- use #triggers
    let conditions =  mconcat $  fmap (\t -> runTrigger t sources action) triggers
    evalCondition conditions -- key here is that evalCondition only uses reader part of state

handleNode :: (Ord l, Ord r) => GameNode l r ph pl -> GameT l r ph pl t tn [GameNode l r ph pl]
handleNode n = let
        sources = mapMaybe (view #source) (n:view #parents n)
        nodeStuff = view #node n
        in either chooseNode (act >> getTriggers sources) nodeStuff

runNode :: (Ord l, Ord r) => GameNode l r ph pl -> GameT l r ph pl t tn ()
runNode n = do
    result <- handleNode n
    traverse_ runNode result

data Phase phaseName l r playName t tn = Phase {
    name :: phaseName,
    enterAction :: Condition l r phaseName playName t tn [GameAction l r phaseName],
    exitAction :: Condition l r phaseName playName t tn [GameAction l r phaseName],
    legal :: playName -> Condition l r phaseName playName t tn Bool,
    control :: PhaseControl
                                      }

---------------- Other stuff ------------------

data PhaseControl = One Player
                | Sequential [Player]
                | Simultaneous [Player]
                | None deriving (Eq, Ord, Show, Generic)

-- Data representations of a Transfer
-- Key interpreter is to transfer function
-- stuff source target
data Transfer lname resource = Transfer resource lname lname deriving (Eq, Ord, Show, Generic)


