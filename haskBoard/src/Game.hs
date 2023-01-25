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
import Count
-- import Control.Monad.Random.Class
import System.Random (uniformR)
import FinitaryMap (ftAt)
import Data.Finitary
import Control.Monad ( join )

-- Need to define some types before Game.

-- These are the fundamental actions in a game. All the "verbs" of a game (besides the observations, e.g. "check" and "count") should be phrase in terms of these.
data GameAction l cn r ph = DoNothing
    | MkTransfer l l r
    | IncrementCounter cn
    | DecrementCounter cn
    | SetCounter cn (Cnt Int)
    | RollCounter cn
    | ChangePhase ph
    | EndGame
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



mkActionNode :: GameAction l cn r ph -> GameNode l cn r ph pl t tn
mkActionNode action = GameNode (Right [action]) Nothing Nothing []

mkActionNodeS :: GameAction l cn r ph -> GameS l cn r ph pl t tn (GameNode l cn r ph pl t tn)
mkActionNodeS = pure . mkActionNode

mkActionNodeL :: [GameAction l cn r ph] -> GameNode l cn r ph pl t tn
mkActionNodeL action = GameNode (Right action) Nothing Nothing []

mkChoiceNode :: Player -> Choice l cn r ph pl t tn -> GameNode l cn r ph pl t tn
mkChoiceNode p choice = GameNode (Left choice) Nothing (Just p) []


data Phase phaseName l cn r playName t tn = Phase {
    name :: phaseName,
    seedNodes :: [GameNode l cn r phaseName playName t tn]
  }


-- For now, `Game` is a big record of functions
-- Could be replaced by something more monadic.
-- Define a State type right below.
data Game l cn r phaseName playName turns triggerName = Game
  { players :: [Player],
    objects :: GameObjects l cn r,
    runPlay ::  playName -> GameS l cn r phaseName playName turns triggerName [GameNode l cn r phaseName playName turns triggerName],
    randGen :: StdGen,
    chooser :: Choice l cn r phaseName playName turns triggerName -> playName,
    currentStack :: [GameNode l cn r phaseName playName turns triggerName],
    -- triggers :: [Trigger l cn r phaseName playName turns triggerName],
    activePlayer :: Maybe Player,
    turnNumber :: turns,
    currentPhase :: phaseName,
    phases :: phaseName -> Phase phaseName l cn r playName turns triggerName
  }
  deriving (Generic)

getRunPlay :: pl ->  GameS l cn r ph pl t tn [GameNode l cn r ph pl t tn]
getRunPlay play = join (use #runPlay <*> pure play)

type GameS l cn r ph pl t tn = State (Game l cn r ph pl t tn)

makeFields ''Game

instance RandomGen (Game l cn r phaseName playName turns triggerName) where
    split game = let (gen, gen') = split (game ^. #randGen)
                  in (game & #randGen .~ gen, game & #randGen .~ gen')
    genWord32 game = let (out, gen') = genWord32 (game ^. #randGen)
                    in (out, game & #randGen .~ gen')


data ControlAction = None | Terminate deriving (Eq, Ord, Generic)

noControl :: Monad m => m () -> m ControlAction
noControl x = x >> pure None

terminateAfter :: Monad m => m () -> m ControlAction
terminateAfter x = x >> pure Terminate

act :: (Ord l, Ord r, Ord cn, Finitary cn) => GameAction l cn r phaseName -> GameS l cn r phaseName playName turns triggerName ControlAction
act DoNothing = noControl $ return ()
act (MkTransfer l l' r) = noControl $ modifying (#objects . #locations) (transfer r l l')
act (IncrementCounter c) = noControl $ modifying (#objects . #counters . ftAt c) increment
act (DecrementCounter c) = noControl $ modifying (#objects . #counters . ftAt c) decrement
act (SetCounter c v) = noControl $ modifying (#objects . #counters . ftAt c) (`setCounter` v)
act (RollCounter c) = noControl $ do
    (bl, bu) <- gets (view (#objects . #counters . ftAt c . #bounds))
    newVal <- state (uniformR (bl,bu))
    assign (#objects . #counters . ftAt c . #val)  newVal
act (ChangePhase ph) = terminateAfter $ do
                        assign #currentPhase ph
                        newPhase <- uses #phases ($ ph)
                        assign #currentStack (seedNodes newPhase)
act EndGame = undefined


-- could rewrite GameS in this style?
-- class Monad m => MonadChoice m pl where
--     choose :: Choice pl -> m pl

-- But for now use this, basically ReaderT pattern.
choosePlay :: Choice l cn r ph pl t tn -> (GameS l cn r ph pl t tn) pl
choosePlay c =  do
        chooser <- use #chooser
        return (chooser c)

-- Given a `Choice`, create the appropriate Actions and decisions
chooseNode :: Choice l cn r ph pl t tn -> GameS l cn r ph pl t tn [GameNode l cn r ph pl t tn]
chooseNode c = choosePlay c >>= getRunPlay

-- Need to be able to stop computing for e.g. phase change.
runNodes :: (Ord l, Ord r, Ord cn, Finitary cn) => [GameNode l cn r ph pl t tn] -> GameS l cn r ph pl t tn ()
runNodes [] = pure () -- don't let this happen!
runNodes (aNode:rest) = case view #node aNode of
    Left aChoice -> do
        chosen <- chooseNode aChoice
        runNodes (chosen ++ rest)
    Right anAction -> do
        controlAction <- actControl anAction -- traverse_ actGo anAction >> runNodes rest
        case controlAction of 
          None -> runNodes rest
          Terminate -> pure ()
    where
       actControl :: (Ord l, Ord r, Ord cn, Finitary cn) => 
           [GameAction l cn r ph] -> GameS l cn r ph pl t tn ControlAction
       actControl [] = pure None
       actControl (action:moreActions) = do
           control <- act action
           case control of
             None -> actControl moreActions
             Terminate -> pure Terminate
        

--
--
---------------- Other stuff ------------------

-- Data representations of a Transfer
-- Key interpreter is to transfer function
-- stuff source target
data Transfer lname resource = Transfer resource lname lname deriving (Eq, Ord, Show, Generic)


