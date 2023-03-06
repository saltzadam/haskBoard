{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# LANGUAGE FunctionalDependencies #-}

module GameE where

import Control.Lens
    ( Getting, makeFields, over, view, (^.), to, Lens', set )
import Control.Lens.Combinators (ASetter)
import Count
import Effectful
import Effectful.Crypto.RNG
  ( CryptoRNG (..),
    RNG (..),
    newCryptoRNGState,
    runCryptoRNG,
  )
import Effectful.Dispatch.Dynamic (interpret, send)
import qualified Effectful.Reader.Static as R
import qualified Effectful.State.Static.Local as S
import FinitaryMap (ftAt)
import GHC.Generics (Generic)
import Location ( decrement, increment, setCounter, transfer, inventory, GameObjects, Counter, LocationShape)
import Data.Finitary (Finitary)
import Data.Bitraversable (bitraverse)
import Data.Tree (Tree (..))
import Data.Set (Set)
import Game.Player (Player)
import Control.Applicative
import Control.Monad.Trans.Maybe (MaybeT(..))
import Effectful.Log (Log, logInfo, runLog, defaultLogLevel)
import qualified Data.Text as T
import Log.Backend.StandardOutput
import qualified Data.List.NonEmpty as NE
import Game.Options
import TreeControl
import Effectful.Dispatch.Static (StaticRep, SideEffects(..), getStaticRep, evalStaticRep, putStaticRep, runStaticRep, stateStaticRepM)

-- TODO: export list

-- TODO: (lower) abstract out log
-- TODO: (fun) some kind of history besides log
data GameNode l cn r ph pl i = GameNode
  { node :: Either  (GameAction l cn r ph) (GetOptions l cn r ph pl i),
    owner :: Maybe Player
  }
  deriving (Generic)

-- Need to think more carefully about the separation of data and rules
--  Phase has some kind of seedNodes, current [playName]
--  GameRules can turn those into [GameNode]
--  But that's not right: needs to be something like Either playName [playName]
--  Left means do it, right means choice
-- This is also bad -- rolling dice shouldn't be a playName
--
-- Before, Phase had seedNodes :: [GameNode]
-- This caused some kind of loop:
--  b = seedNodes $ (phases gameRules) (Turn (Player 0))
--  fmap owner . runPureEff . R.runReader (initGameData 3) . R.runReader (gameRules) $ b
--  ^ hangs
--  there is some circularity here
--
--  Conceptually, the issue is that seedNodes really should be GameNodes.
--  But player choices should be PlayName. 


data Phase phaseName l cn r playName i = Phase {
    name :: phaseName,
    -- seedNodes :: ObserveGame l cn r ph iaseName [GameNode l cn r phaseName playName ]
    seedNodes :: ObserveGame l cn r phaseName playName i [GameNode l cn r phaseName playName i]
  } deriving (Generic)

getPhaseNodes :: Phase phaseName l cn r playName i -> ObserveGame
     l cn r phaseName playName i [GameNode l cn r phaseName playName i]
getPhaseNodes (Phase _ seedNodes) = seedNodes

-- thanks /u/typedbyte
-- https://www.reddit.com/r/haskell/comments/10ql43j/monthly_hask_anything_february_2023/jabrmxk/

data Mode = Observe | Modify deriving (Eq, Ord, Show, Generic)
-- data Data (mode :: Mode) (a :: Type) :: Effect
-- type instance DispatchOf (Data mode a) = Static NoSideEffects
-- newtype instance StaticRep (Data mode a) = Data a

-- testRead :: (Data Get a :> es) => Eff es a
-- testRead = do
--     Data a <- getStaticRep 
--     pure a

data Game l cn r ph pl i = Game { gameState :: GameState l cn r ph,
                                  gamerules :: GameRules l cn r ph pl i}
                                                deriving (Generic)

data GameInteract (mode :: Mode) l cn r ph pl i :: Effect
type instance DispatchOf (GameInteract mode l cn r ph pl i) = Static NoSideEffects
newtype instance StaticRep (GameInteract mode l cn r ph pl i) = GameInteract (Game l cn r ph pl i)

liftStaticRepMode :: StaticRep (GameInteract Observe l cn r ph pl i) -> StaticRep (GameInteract Modify l cn r ph pl i)
liftStaticRepMode (GameInteract g) = GameInteract g


maybeUnsafeUnliftStaticRepMode :: StaticRep (GameInteract Modify l cn r ph pl i) -> StaticRep (GameInteract Observe l cn r ph pl i)
maybeUnsafeUnliftStaticRepMode (GameInteract g) = GameInteract g

runGameInteract :: GameState l cn r ph -> GameRules l cn r ph pl i -> Eff (GameInteract mode l cn r ph pl i : es) a -> Eff es a
runGameInteract gd gr = evalStaticRep (GameInteract (Game gd gr))

askGame :: GameInteract mode l cn r ph pl i :> es => Eff es (GameState l cn r ph)
askGame = do
    GameInteract (Game gd _) <- getStaticRep
    return gd

askRules :: GameInteract mode l cn r ph pl i :> es => Eff es (GameRules l cn r ph pl i)
askRules = do
    GameInteract (Game _ gr) <- getStaticRep
    return gr

runObserve ::  Eff (GameInteract Observe l cn r ph pl i : es) a -> (StaticRep (GameInteract 'Observe l cn r ph pl i)) -> Eff es (a,
                   (StaticRep (GameInteract 'Observe l cn r ph pl i)))
runObserve eff gi = runStaticRep (gi) eff

liftObserve :: (GameInteract 'Modify l cn r ph pl i :> es) => Eff (GameInteract 'Observe l cn r ph pl i : es) a -> Eff es a
liftObserve eff = stateStaticRepM (fmap (fmap liftStaticRepMode) . runObserve eff . maybeUnsafeUnliftStaticRepMode)


-- TODO: is there a clean way to restrict state to reader?
type GameStateS l cn r ph pl i es = (GameInteract Modify l cn r ph pl i :> es)
type GameStateR l cn r ph = R.Reader (GameState l cn r ph)
type GameRulesR l cn r ph pl i = R.Reader (GameRules l cn r ph pl i)
type ObserveGame l cn r ph pl i = Eff '[ GameInteract Observe l cn r ph pl i, Log]
type ModifyGame l cn r ph pl i = Eff '[ GameInteract Modify l cn r ph pl i, Log]

-- observeGame ::  (Subset es' es, GameStateS l cn r ph pl i es) =>  Eff (GameStateR l cn r ph:es') a -> Eff es a
-- observeGame eff = S.stateM (\r -> inject $ fmap (,r) (R.runReader r eff))

-- observeToModify :: (Subset es' es, GameStateS l cn r ph pl i es) => Eff (ObserveGame l cn r ph pl i:es') a -> Eff es a
-- observeToModify eff = go eff where
--     go eff = \gameRules -> S.stateM (\gameData -> (fmap ((, gameData) . getData) . evalStaticRep (Game Observe (Observation gameData gameRules))) eff)
--     getData (Game gd _) = gd
--     getRules (Game _ gr) = gr
--     test eff gd gr = evalStaticRep (Game Observe (Observation gd gr)) eff

-- observeGame ::  (Subset es' es, GameStateS l cn r ph :> es) =>  Eff (Game Observe l cn r ph pl i:es') a -> Eff es a
-- observeGame eff = S.stateM (\r -> 
--     where

data GameState l cn r ph = GameState
  { players :: Set Player,
    objects :: GameObjects l cn r,
    currentPhase :: ph
  }
  deriving (Generic)

data GameRules l cn r ph pl i = GameRules
  { phases :: ph -> Phase ph l cn r pl i,
    runPlay :: pl -> ObserveGame l cn r ph pl i [GameNode l cn r ph pl i]
  }
  deriving (Generic)

-- These are the fundamental actions in a game. All the "verbs" of a game (besides the observations, e.g. "check" and "count") should be phrase in terms of these.
data GameAction l cn r ph
  = DoNothing
  | MkTransfer l l r
  | IncrementCounter cn
  | DecrementCounter cn
  | SetCounter cn (Cnt Int)
  | RollCounter cn
  | ChangePhase ph
  | EndGame
  deriving (Eq, Ord, Show, Generic)

modifyGameState :: forall l cn r ph pl i es . (GameInteract Modify l cn r ph pl i :> es) => (GameState l cn r ph  -> GameState l cn r ph) -> Eff es ()
modifyGameState f = do
    GameInteract (Game gs gr) <- getStaticRep @(GameInteract Modify l cn r ph pl i)
    putStaticRep (GameInteract (Game (f gs) gr))

modifyingGameState :: (GameInteract Modify l cn r ph pl i :> es) => ASetter (GameState l cn r ph) (GameState l cn r ph) a b -> (a -> b) -> Eff es ()
modifyingGameState o = modifyGameState . over o


assignGameState :: (GameInteract Modify l cn r ph pl i :> es) => ASetter (GameState l cn r ph) (GameState l cn r ph) a b -> b -> Eff es ()
assignGameState l b = modifyGameState (set l b)

getsGameState :: forall mode l cn r ph pl i es b . (GameInteract mode l cn r ph pl i :> es) => (GameState l cn r ph -> b) -> Eff es b
getsGameState f = do
    GameInteract (Game gs _) <- getStaticRep @(GameInteract mode l cn r ph pl i)
    return (f gs)

getsGameRules :: forall mode l cn r ph pl i es b . (GameInteract mode l cn r ph pl i :> es) => (GameRules l cn r ph pl i -> b) -> Eff es b
getsGameRules f = do
    GameInteract (Game _ gr) <- getStaticRep @(GameInteract mode l cn r ph pl i)
    return (f gr)

getGameState ::  forall mode l cn r ph pl i es . (GameInteract mode l cn r ph pl i :> es) => Eff es (GameState l cn r ph)
getGameState = getsGameState id

getGameRules :: forall mode l cn r ph pl i es . (GameInteract mode l cn r ph pl i :> es) => Eff es (GameRules l cn r ph pl i)
getGameRules = getsGameRules id

useGameState :: (GameInteract mode l cn r ph pl i :> es) => (Getting b (GameState l cn r ph) b) -> Eff es b
useGameState o = getsGameState (view o)

useGameRules :: (GameInteract mode l cn r ph pl i :> es) => (Getting b (GameRules l cn r ph pl i) b) -> Eff es b
useGameRules o = getsGameRules (view o)



data (GameControl ph) = Continue | ChangePhaseTo ph | End deriving (Eq, Ord, Show, Generic)

continueGame :: Eff es (GameControl ph)
continueGame = return Continue

showInventory :: (GameInteract Observe l cn r ph pl i :> es, Eq l, Show r, Ord r) => l -> Eff es String
showInventory l = show . inventory <$> useGameState (locationByName l)


logAction :: (Show r, Show l, Show cn, Show ph, Show val, Ord r, Eq l) => GameAction l cn r ph -> val -> Eff '[GameInteract Observe l cn r ph pl i, Log] ()
logAction DoNothing _ = pure ()
logAction (MkTransfer l l' r) val = do
                                invl <- showInventory l
                                invl' <- showInventory l'
                                logInfo (T.pack $ "Transfered " ++ show r ++ " from " ++ show l ++ " to " ++ show l'
                                   ++ "\n Contents of " ++ show l ++ ": " ++ invl
                                   ++ "\n Contents of " ++ show l' ++ ": " ++ invl') (show val)
logAction (IncrementCounter cn) val = logInfo (T.pack $ "Incremented " ++ show cn) (show val)
logAction (DecrementCounter cn) val = logInfo (T.pack $ "Decremented " ++ show cn) (show val)
logAction (SetCounter cn i) val = logInfo (T.pack $ "Set " ++ show cn ++ " to " ++ show i) (show val)
logAction (RollCounter cn) val = logInfo (T.pack $ "Rolled " ++ show cn) (show val)
logAction (ChangePhase ph) val = logInfo (T.pack $ "Changed phase to " ++ show ph) (show val)
logAction EndGame val = logInfo "Ended game" (show val)

logAction' :: t1 -> t2 -> Eff xs a
logAction' action val = inject (logAction' action val)


act :: (Ord l, Ord r, RNG :> es, GameStateS l cn r ph pl i es,  Eq cn, Log :> es, Show ph, Show cn, Show l, Show r) => GameAction l cn r ph -> Eff es (GameControl ph)
act DoNothing = continueGame
act a@(MkTransfer l l' r) = modifyingGameState (#objects . #locations) (transfer r l l')
                            >> (logAction' a ' ')
                            >> continueGame
act a@(IncrementCounter c) = modifyingGameState (counterByName c) increment
                            >> useGameState (counterByName c . #val) >>= logAction' a
                            >> continueGame
act a@(DecrementCounter c) = modifyingGameState (counterByName c) decrement
                            >> useGameState (counterByName c . #val) >>= logAction' a
                            >> continueGame
act a@(SetCounter c v) = modifyingGameState (counterByName c) (`setCounter` v)
                            >> useGameState (counterByName c . #val) >>= logAction' a
                            >> continueGame
act a@(RollCounter c) = do
  (bl, bu) <- useGameState (counterByName c . #bounds)
  newVal <- randomR (bl, bu)
  assignGameState (counterByName c . #val) newVal
  _ <- useGameState (counterByName c . #val) >>= logAction' a
  continueGame
act a@(ChangePhase ph) = assignGameState #currentPhase ph
                            >> (logAction' a (show ph))
                            >> return (ChangePhaseTo ph)
act EndGame = (logAction' EndGame ' ') >> return End

-- The flow of a game looks like this: there is some sequence of `GameActions` (draw a card, advance the turn counter) until a player must make a `GetOptions`. GetOptionss produce sequences of actions and additional choices, and so on.

type GetOptions l cn r ph pl i = ObserveGame l cn r ph pl i (Options pl i)

counterByName :: Eq cn => cn -> Lens' (GameState l cn r ph) Counter
counterByName c = #objects . #counters . ftAt c

locationByName :: Eq l => l -> Lens' (GameState l cn r ph) (LocationShape r)
locationByName l = #objects . #locations . ftAt l


makeFields ''GameRules
makeFields ''GameState
makeFields ''Game
makeFields ''Phase

mkActionNode :: GameAction l cn r ph -> GameNode l cn r ph pl i
mkActionNode action = GameNode (Left action) Nothing

mkGetOptionsNode :: Player -> GetOptions l cn r ph pl i -> GameNode l cn r ph pl i
mkGetOptionsNode p choice = GameNode (Right choice) (Just p)


chooseNode :: forall l cn r ph pl i es. (Choosing :> es, GameInteract Observe l cn r ph pl i :> es, Log :> es, Show pl, Show i) =>  GetOptions l cn r ph pl i
        -> Eff es [GameNode l cn r ph pl i]
chooseNode cs =
  let cs' = inject cs
   in do
        options <- cs'
        logInfo (T.pack ("Choosing from " ++ show options)) ' '
        playRunner <- useGameRules #runPlay
        c <- choose options
        logInfo (T.pack ("Chose " ++ show c)) ' '
        inject (playRunner c)

data Choosing :: Effect where
  Choose :: GameState l cn r ph -> Options pl i -> Choosing m pl

type instance DispatchOf Choosing = 'Dynamic

choose :: forall l cn r ph pl es i. (Choosing :> es, GameInteract Observe l cn r ph pl i :> es) => Options pl i -> Eff es pl
choose cs = askGame >>= \g -> send (Choose g cs)

chooseFirst :: forall es pl . Eff (Choosing : es) pl -> Eff es pl
chooseFirst = interpret $ \_ -> \case
  Choose _ cs -> return (cs ^. #legal . to NE.head)

chooseRandom :: (RNG :> es) => Eff (Choosing : es) pl -> Eff es pl
chooseRandom = interpret $ \_ -> \case
  Choose _ cs' ->
    let cs = cs' ^. #legal
        choice = randomR (0, length cs - 1)
     in fmap (cs NE.!!) choice


runNode :: forall l r cn ph pl es i. (Ord l, Ord r, Ord cn, Finitary cn, Choosing :> es, GameStateS l cn r ph pl i es, RNG :> es, Log :> es, Show ph, Show cn, Show l, Show r, Show pl, Show i) => GameNode l cn r ph pl i -> Eff es (Either  (GameControl ph) [GameNode l cn r ph pl i])
runNode aNode = bitraverse act (liftObserve . chooseNode) $ view #node aNode


runFromSeeds :: forall l r cn ph pl es i. (Ord l, Ord r, Ord cn, Finitary cn, GameStateS l cn r ph pl i es, Choosing :> es, RNG :> es, Log :> es, Show ph, Show cn, Show l, Show r, Show pl, Show i) => [GameNode l cn r ph pl i] -> MaybeT (Eff es) [Tree (GameNode l cn r ph pl i)]
runFromSeeds = unfoldForestControl treefunc
    where
        treefunc :: GameNode l cn r ph pl i -> MaybeT (Eff es) (GameNode l cn r ph pl i, [GameNode l cn r ph pl i], TreeControl (GameNode l cn r ph pl i))
        treefunc nod = MaybeT $ do
                    result <- runNode nod
                    case result of
                      Left Continue ->  return $ Just (nod, [], TContinue)
                      Left End -> return Nothing
                      Left (ChangePhaseTo ph) -> do
                            phases <- inject . liftObserve . useGameRules $ #phases
                            newNodes <-  liftObserve . inject . getPhaseNodes $ (phases ph)
                            return $ Just (nod, [], TRestart newNodes)
                      Right moreNodes -> return $ Just (nod, moreNodes, TContinue)

playGame ::  forall l cn r ph pl es i. (Ord l, Ord r, Ord cn, Finitary cn, RNG :> es, Choosing :> es, GameStateS l cn r ph pl i es, Log :> es, Show ph, Show cn, Show l, Show r, Show pl, Show i) => Eff es (GameState l cn r ph)
playGame = do
    phases <- useGameRules #phases
    currentPhase <- useGameState #currentPhase
    newNodes <- liftObserve . inject . getPhaseNodes $ (phases currentPhase)
    _ <- runMaybeT . runFromSeeds $ newNodes
    getGameState

action :: (Ord l, Ord r, Ord cn, Finitary cn, Show ph, Show cn, Show l, Show r, Show pl, Show i) => GameState l cn r ph -> GameRules l cn r ph pl i -> IO (GameState l cn r ph)
action gdata grules = do
  gen <- newCryptoRNGState
  withStdOutLogger $ \stdOutLogger -> runEff .  runGameInteract gdata grules . runCryptoRNG gen . chooseRandom . runLog "main" stdOutLogger defaultLogLevel $ playGame

----- Condition, perhaps soon to be Observation?
type Condition l cn r ph pl i a = ObserveGame l cn r ph pl i a

runNodesAgainstState :: (Ord l, Ord r, Ord cn, Finitary cn, Show ph, Show cn, Show l, Show r, Show pl, Show i) => GameState l cn r ph -> GameRules l cn r ph pl i -> [GameNode l cn r ph pl i] -> IO (GameState l cn r ph)
runNodesAgainstState gd grules nodes = do
    gen <- newCryptoRNGState
    withStdOutLogger $ \stdOutLogger -> runEff . runGameInteract gd grules . runCryptoRNG gen . chooseRandom . runLog "main" stdOutLogger defaultLogLevel . S.execState gd $ (runMaybeT . runFromSeeds $ nodes)

instance Num a => Num (ObserveGame l cn r ph pl i a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = return . fromInteger
  negate = fmap negate

