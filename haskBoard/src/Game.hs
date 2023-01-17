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
-- {-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
module Game where

-- import Data.Finitary
import GHC.Generics (Generic)
import Game.Player
import Location
import Text.Show.Functions ()
import Control.Monad.Random (StdGen, RandomGen (..))
import Control.Lens hiding (Empty, Choice)
import Control.Applicative
import Control.Monad.Trans.State
import Data.Foldable (traverse_)
import Count
-- import Control.Monad.Random.Class
import System.Random (uniformR)
import FinitaryMap (ftAt)
import Data.Finitary
import Data.Bitraversable
import Control.Monad (join, ap)
import Control.Monad ((>=>))

-- Need to define some types before Game.

-- These are the fundamental actions in a game. All the "verbs" of a game (besides the observations, e.g. "check" and "count") should be phrase in terms of these.
data GameAction l cn r ph = DoNothing
    | MkTransfer l l r
    | IncrementCounter cn
    | DecrementCounter cn
    | SetCounter cn (Cnt Int)
    | RollCounter cn
    | ChangePhase ph
    deriving (Eq, Ord, Show, Generic)

type GameActionS l cn r ph pl t tn = GameS l cn r ph pl t tn (GameAction l cn r ph)

-- Computations within a game which produce `a`.
-- Want a good interace here so that we can evaluate, pretty-print, parse/validate, etc.
-- Don't have that yet.
newtype Condition l cn r ph pl t tn a = Condition {runCondition :: GameS l cn r ph pl t tn a}
    deriving (Functor, Applicative, Monad, Generic)

instance Semigroup a => Semigroup (Condition l cn r ph pl t tn a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (Condition l cn r ph pl t tn a) where
    mempty = return mempty

-- Compute a condition given a `Game`.
-- Shows redunacy of current definition?
evalCondition ::  Condition l cn r ph pl t tn a  -> (GameS l cn r ph pl t tn) a
evalCondition = view #runCondition

-- To make Conditions easier to work with
instance Num a => Num (Condition l cn r ph pl t tn a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)
    abs = fmap abs
    signum = fmap signum
    fromInteger  = pure . fromInteger



-- A play `pl` is just a choice that a player must make. `Choice` is a set of plays
-- to be presented to a player.
type Choice l cn r ph pl t tn = GameS l cn r ph pl t tn [pl]



-- The flow of a game looks like this: there is some sequence of `GaAeActions` (draw a card, advance the turn counter) until a player must make a `Choice`. Choices produce sequences of actions and additional choices, and so on. Also, 'GameAction` can indirectly produce choices via `Triggers`. For those `Triggers`, it's important to keep track of the parent actions and sources. Putting all of this together, we get a tree. The nodes are `GameNode`s.
--
-- `source` is a kind of shorthand -- just to make sure that triggers do not trigger themselves, for example.
data GameNode l cn r ph pl t tn = GameNode {
        -- priority :: Int, -- don't need this yet
        node :: Either (Choice l cn r ph pl t tn) [GameAction l cn r ph],
        source :: Maybe (l,r),
        owner :: Maybe Player,
        parents :: [GameNode l cn r ph pl t tn]
                                   } deriving (Generic)



-- -- `Triggers` are checked after each action.
-- -- The main thing is the `condition`. Given an action and a list of sources (immediate source at head)
-- -- and an action, should the trigger fire? If so, it will produce `GameNodes`.
-- data Trigger l cn r ph pl t name = Trigger { condition :: [(l,r)] -> GameAction l cn r ph -> Condition l cn r ph pl t name [GameNode l cn r ph pl], -- should be NE list of sources
--                                      name :: name,
--                                      source :: (l,r)
--                                      -- prioirty :: Int
--                                    } deriving (Generic)

-- runTrigger :: Trigger l cn r ph pl t name -> [(l,r)] -> GameAction l r ph -> Condition l cn r ph pl t name [GameNode l cn r ph pl]
-- runTrigger = view #condition

-- instance Show name => Show (Trigger l cn r ph pl t name) where
--     show t = show (t ^. #name)

-- For now, `Game` is a big record of functions
-- Could be replaced by something more monadic.
-- Define a State type right below.
data Game l cn r phaseName playName turns triggerName = Game
  { players :: [Player],
    objects :: GameObjects l cn r,
    runPlay ::  playName -> GameS l cn r phaseName playName turns triggerName [GameNode l cn r phaseName playName turns triggerName],
    randGen :: StdGen,
    chooser :: Choice l cn r phaseName playName turns triggerName -> playName,
    -- legalMoves :: Choice l cn r phaseName playName turns triggerName
    -- triggers :: [Trigger l cn r phaseName playName turns triggerName],
    activePlayer :: Maybe Player,
    turnNumber :: turns
  }
  deriving (Generic)

getRunPlay :: pl ->  GameS l cn r ph pl t tn [GameNode l cn r ph pl t tn]
getRunPlay play = join (use #runPlay <*> pure play)

type GameS l cn r ph pl t tn = State (Game l cn r ph pl t tn)

-- -- withRandGen :: (Monad m, Data.Generics.Product.Fields.HasField' "randGen" s a1) => (a1 -> (a2, s)) -> StateT s m a2
-- withRandGen :: (StdGen -> (a, StdGen)) -> GameS l cn r ph pl t tn a
-- withRandGen f = do
--     gen <- use #randGen
--     let (a, gen') = f gen
--     assign #randGen gen'
--     return a

-- instance MonadRandom (GameS l cn r ph pl t tn) where
--     getRandomR lohi = withRandGen (randomR lohi)
--     getRandom = withRandGen random
--     getRandomRs lohi =  randomRs lohi <$> use #randGen
--     getRandoms = randoms <$> use #randGen

makeFields ''Game

instance RandomGen (Game l cn r phaseName playName turns triggerName) where
    split game = let (gen, gen') = split (game ^. #randGen)
                  in (game & #randGen .~ gen, game & #randGen .~ gen')
    genWord32 game = let (out, gen') = genWord32 (game ^. #randGen)
                    in (out, game & #randGen .~ gen')


-- instance MonadRandom (GameS l cn r ph pl t tn) where
--     getRandomR (bl,bu) = state (randomR (bl,bu))
--     getRandom = state random
--     getRandomRs (bl,bu) = do
--         gen <- use #randGen
--         return $ randomRs (bl,bu) gen 
--     getRandoms = do
--         gen <- use #randGen
--         return $ randoms gen


act :: (Ord l, Ord r, Ord cn, Finitary cn) => GameAction l cn r phaseName -> GameS l cn r phaseName playName turns triggerName ()
act DoNothing = return ()
act (MkTransfer l l' r) = modifying (#objects . #locations) (transfer r l l')
act (IncrementCounter c) = modifying (#objects . #counters . ftAt c) increment
act (DecrementCounter c) = modifying (#objects . #counters . ftAt c) decrement
act (SetCounter c v) = modifying (#objects . #counters . ftAt c) (`setCounter` v)
act (RollCounter c) = do
    (bl, bu) <- gets (view (#objects . #counters . ftAt c . #bounds))
    newVal <- state (uniformR (bl,bu))
    assign (#objects . #counters . ftAt c . #val)  newVal
act (ChangePhase ph) = undefined -- TODO: while we figure out control flow


-- could rewrite GameS in this style?
-- class Monad m => MonadChoice m pl where
--     choose :: Choice pl -> m pl

-- But for now use this, basically ReaderT pattern.
choosePlay :: Choice l cn r ph pl t tn -> (GameS l cn r ph pl t tn) pl
choosePlay c =  do
        g <- get
        let chooser = view #chooser g
        return (chooser c)

-- Given a `Choice`, create the appropriate Actions and decisions
-- TODO: Triggers should pick up plays as well.
chooseNode :: Choice l cn r ph pl t tn -> GameS l cn r ph pl t tn [GameNode l cn r ph pl t tn]
chooseNode c = choosePlay c >>= getRunPlay

-- -- Evaluate all the triggers on a particular instance of a `GameAction`.
-- -- TODO: As above, Triggers should pick up plays/choices as well.
-- getTriggers :: [(l,r)] -> GameAction l cn r ph -> GameS l cn r ph pl t tn [GameNode l cn r ph pl]
-- getTriggers sources action = do
--     triggers <- use #triggers
--     let conditions =  mconcat $  fmap (\t -> runTrigger t sources action) triggers
--     evalCondition conditions -- key here is that evalCondition only uses reader part of state

handleNode :: (Ord l, Ord r, Ord cn, Finitary cn) => GameNode l cn r ph pl t tn -> GameS l cn r ph pl t tn (Either [GameNode l cn r ph pl t tn] ())
handleNode n =  bitraverse chooseNode (traverse_ act) (view #node n)

runNode :: (Ord l, Ord r, Ord cn, Finitary cn) => GameNode l cn r ph pl t tn -> GameS l cn r ph pl t tn ()
runNode n = do
    result <- handleNode n 
    case result of
      Left nodes -> traverse_ runNode nodes
      Right _ -> return ()

data Phase phaseName l cn r playName t tn = Phase {
    name :: phaseName,
    seedNodes :: [GameNode l cn r phaseName playName t tn]
  }

---------------- Other stuff ------------------

-- Data representations of a Transfer
-- Key interpreter is to transfer function
-- stuff source target
data Transfer lname resource = Transfer resource lname lname deriving (Eq, Ord, Show, Generic)


