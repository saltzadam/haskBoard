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
{-# HLINT ignore "Use tuple-section" #-}
module Game where

-- import Data.Finitary
import GHC.Generics (Generic)
import Location
import Text.Show.Functions ()
import Control.Monad.Random (StdGen, RandomGen (..))
import Control.Lens hiding (Empty, Choice)
import Control.Applicative
import Control.Monad.Trans.State.Lazy
import Count
-- import Control.Monad.Random.Class
import System.Random (uniformR)
import FinitaryMap (ftAt)
import Data.Finitary
import Control.Monad ( join )
import Data.Bitraversable (bitraverse)
import Data.Tree (Tree(..), unfoldForestM)
import GHC.Natural (Natural)

-- Need to define some types before Game.

initGame
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

-- A play `pl` is just a choice that a player must make. `Choice` is a set of plays
-- to be presented to a player.
type Choice l cn r ph pl = GameS l cn r ph pl [pl]

type Chooser l cn r ph pl = GameS l cn r ph pl [pl] -> GameS l cn r ph pl pl


choose  :: Choice l cn r ph pl -> GameS l cn r ph pl pl
choose choice =  gets chooser >>= ($ choice)

-- The flow of a game looks like this: there is some sequence of `GameActions` (draw a card, advance the turn counter) until a player must make a `Choice`. Choices produce sequences of actions and additional choices, and so on. Also, 'GameAction` can indirectly produce choices via `Triggers`. For those `Triggers`, it's important to keep track of the parent actions and sources.
--
-- `source` is a kind of shorthand -- just to make sure that triggers do not trigger themselves, for example.
data GameNode l cn r ph pl = GameNode {
        -- priority :: Int, -- don't need this yet
        node :: Either (Choice l cn r ph pl ) (GameAction l cn r ph),
        -- sourceL :: Maybe l, -- premature
        -- sourceR :: Maybe r,
        owner :: Maybe Natural
        -- parents :: [GameNode l cn r ph pl t pls]
                                   } deriving (Generic)

mkActionNode :: GameAction l cn r ph -> GameNode l cn r ph pl 
mkActionNode action = GameNode (Right action) Nothing

mkChoiceNode :: Natural -> Choice l cn r ph pl -> GameNode l cn r ph pl 
mkChoiceNode p choice = GameNode (Left choice) (Just p)

data Phase phaseName l cn r playName = Phase {
    name :: phaseName,
    seedNodes :: [GameNode l cn r phaseName playName ]
  } deriving (Generic)


-- For now, `Game` is a big record of functions
-- Could be replaced by something more monadic.
-- Define a State type right below.
data Game l cn r phaseName playName = Game
  { players :: Set Natural,
    objects :: GameObjects l cn r,
    runPlay ::  playName -> GameS l cn r phaseName playName [GameNode l cn r phaseName playName],
    randGen :: StdGen,
    chooser :: Chooser l cn r phaseName playName ,
    -- No longer need this -- the currentPhase tells you how things start, then unfolding the tree finishes things off
    -- ChangePhase terminates the current computation and sets a new set of seedNodes to unfold!
    -- currentStack :: [Tree (GameNode l cn r phaseName playName turns players)],
    currentPhase :: phaseName,
    phases :: phaseName -> Phase phaseName l cn r playName 
  }
  deriving (Generic)

getRunPlay :: pl ->  GameS l cn r ph pl [GameNode l cn r ph pl ]
getRunPlay play = join (use #runPlay <*> pure play)

type GameS l cn r ph pl = State (Game l cn r ph pl )

makeFields ''Game

instance RandomGen (Game l cn r phaseName playName) where
    split game = let (gen, gen') = split (game ^. #randGen)
                  in (game & #randGen .~ gen, game & #randGen .~ gen')
    genWord32 game = let (out, gen') = genWord32 (game ^. #randGen)
                    in (out, game & #randGen .~ gen')


type GameActionS l cn r ph pl t pls = GameS l cn r ph pl (GameAction l cn r ph)

-- Computations within a game which produce `a`.
-- Want a good interace here so that we can evaluate, pretty-print, parse/validate, etc.
-- Don't have that yet.
newtype Condition l cn r ph pl a = Condition {runCondition :: GameS l cn r ph pl a}
    deriving (Functor, Applicative, Monad, Generic)

instance Semigroup a => Semigroup (Condition l cn r ph pl a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (Condition l cn r ph pl a) where
    mempty = return mempty

-- Compute a condition given a `Game`.
-- Shows redunacy of current definition?
evalCondition ::  Condition l cn r ph pl a  -> (GameS l cn r ph pl) a
evalCondition = view #runCondition

-- To make Conditions easier to work with
instance Num a => Num (Condition l cn r ph pl a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)
    abs = fmap abs
    signum = fmap signum
    fromInteger  = pure . fromInteger




-- TODO: need to think about Terminate more carefully.
-- Right now, the stack is just a list, so Terminate kills the whole process
-- Need a smarter way to do this, but overengineering for now.
-- data ControlAction = None
--                    -- | Terminate 
--                    deriving (Eq, Ord, Generic)

-- noControl :: Monad m => m () -> m ControlAction
-- noControl x = x >> pure None

act :: (Ord l, Ord r, Ord cn, Finitary cn) => GameAction l cn r phaseName -> GameS l cn r phaseName pls ()
act DoNothing = return ()
act (MkTransfer l l' r) = modifying (#objects . #locations) (transfer r l l')
act (IncrementCounter c) = modifying (#objects . #counters . ftAt c) increment
act (DecrementCounter c) = modifying (#objects . #counters . ftAt c) decrement
act (SetCounter c v) = modifying (#objects . #counters . ftAt c) (`setCounter` v)
act (RollCounter c) = do
    (bl, bu) <- gets (view (#objects . #counters . ftAt c . #bounds))
    newVal <- state (uniformR (bl,bu))
    assign (#objects . #counters . ftAt c . #val)  newVal
act (ChangePhase ph) = assign #currentPhase ph
act EndGame = undefined

chooseRandomFromList :: [a] -> GameS l cn r ph pl a
chooseRandomFromList list = do
    choice <- state (uniformR (1, length list))
    return (list !! (choice - 1))



-- Given a `Choice`, create the appropriate Actions and decisions
chooseNode :: Choice l cn r ph pl -> GameS l cn r ph pl [GameNode l cn r ph pl ]
chooseNode c = choose c >>= getRunPlay

runNode :: (Ord l, Ord r, Ord cn, Finitary cn) => GameNode l cn r ph pl -> GameS l cn r ph pl [GameNode l cn r ph pl ]
runNode aNode = fmap process . bitraverse chooseNode act $ view #node aNode where
    process :: Either [a] () -> [a]
    process (Left xs) = xs
    process (Right _) = []

-- don't need a "step" version -- just look one node at a time and use laziness! (As long as GameS is lazy.)
-- stack can't examine itself but that's probably fine
runFromSeeds :: (Ord l, Ord r, Ord cn, Finitary cn) => [GameNode l cn r ph pl ]
    -> GameS l cn r ph pl [Tree (GameNode l cn r ph pl )]
runFromSeeds = unfoldForestM (\node -> fmap ((,) node) (runNode node))


-- -- Need to be able to stop computing for e.g. phase change.
-- runNodes :: (Ord l, Ord r, Ord cn, Finitary cn) => [GameNode l cn r ph pl t pls] -> GameS l cn r ph pl t pls ()
-- runNodes [] = pure () -- don't let this happen!
-- runNodes (aNode:rest) = do
--     result <- runNode aNode
--     case result of
--       moreNodes -> runNodes (moreNodes ++ rest) -- can't be runNodes moreNodes >> runNodes rest -- can't terminate
--       -- Right Terminate -> pure ()

-- advanceGame :: (Ord l, Ord r, Ord cn, Finitary cn) => Game l cn r ph pl t pls -> Game l cn r ph pl t pls
-- advanceGame g = execState (runNode (head (g ^. #currentStack))) g

playGame :: (Ord l, Ord r, Ord cn, Finitary cn) => GameS l cn r ph pl ()
playGame = do
    phases <- use #phases
    p <- use #currentPhase
    _ <- runFromSeeds (phases p ^. #seedNodes)
    return ()

--
--
---------------- Other stuff ------------------

-- Data representations of a Transfer
-- Key interpreter is to transfer function
-- stuff source target
data Transfer lname resource = Transfer resource lname lname deriving (Eq, Ord, Show, Generic)


