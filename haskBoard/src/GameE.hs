{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use forM_" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use =<<" #-}

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
import FinitaryMap (ftAt)
import GHC.Generics (Generic)
import Location ( decrement, increment, setCounter, transfer, inventory, GameObjects, Counter, LocationShape)
import Data.Finitary (Finitary)
import Data.Tree (unfoldForestM)
import Data.Set (Set)
import Game.Player (Player)
import Effectful.Log (Log, logInfo, runLog, defaultLogLevel)
import qualified Data.Text as T
import Log.Backend.StandardOutput
import qualified Data.List.NonEmpty as NE
import Game.Options
import Effectful.Dispatch.Static (StaticRep, SideEffects(..), getStaticRep, evalStaticRep, putStaticRep)
import GHC.Base (Applicative (..), join)
import Text.Read (readMaybe)
import Data.Maybe (listToMaybe)

-- TODO: export list

-- TODO: (lower) abstract out log
-- TODO: (fun) some kind of history besides log
data GameNode l cn r ph pl i = GameNode
  { node :: Either  (GameAction l cn r ph) (Options pl i),
    owner :: Maybe Player
  }
  deriving (Generic)

data Phase phaseName l cn r playName i = Phase {
    name :: phaseName,
    seedNodes :: [Eff '[GameInteract Observe l cn r phaseName playName i] [GameNode l cn r phaseName playName i]]
  } deriving (Generic)

getPhaseNodes :: Phase phaseName l cn r playName i -> [Eff '[GameInteract Observe l cn r phaseName playName i] [GameNode l cn r phaseName playName i]]
getPhaseNodes (Phase _ seedNodes) = seedNodes

-- thanks /u/typedbyte
-- https://www.reddit.com/r/haskell/comments/10ql43j/monthly_hask_anything_february_2023/jabrmxk/

data Mode = Observe | Modify deriving (Eq, Ord, Show, Generic)

type PlayRunner' l cn r ph pl i mode = pl -> [Eff '[GameInteract mode l cn r ph pl i] [GameNode l cn r ph pl i]]

type PlayRunner l cn r ph pl i = PlayRunner' l cn r ph pl i Observe

data Game l cn r ph pl i = Game { gameState :: GameState l cn r ph pl i,
                                  playRunner :: PlayRunner l cn r ph pl i
                                } deriving (Generic)

-- | GameInteract effects 

data GameInteract (mode :: Mode) l cn r ph pl i :: Effect
type instance DispatchOf (GameInteract mode l cn r ph pl i) = Static NoSideEffects
newtype instance StaticRep (GameInteract mode l cn r ph pl i) = GameInteract (Game l cn r ph pl i)

liftStaticRepMode :: StaticRep (GameInteract Observe l cn r ph pl i) -> StaticRep (GameInteract Modify l cn r ph pl i)
liftStaticRepMode (GameInteract g) = GameInteract g


maybeUnsafeUnliftStaticRepMode :: StaticRep (GameInteract Modify l cn r ph pl i) -> StaticRep (GameInteract Observe l cn r ph pl i)
maybeUnsafeUnliftStaticRepMode (GameInteract g) = GameInteract g

runGameInteract :: forall l cn r ph pl i mode es a. GameState l cn r ph pl i
                -> PlayRunner l cn r ph pl i
                -> Eff (GameInteract mode l cn r ph pl i : es) a -> Eff es a
runGameInteract gd pr = evalStaticRep (GameInteract (Game gd pr) :: StaticRep (GameInteract mode l cn r ph pl i))

observe :: Game l cn r ph pl i -> Eff '[GameInteract mode l cn r ph pl i] a -> a
observe (Game gd pr)  = runPureEff . runGameInteract gd pr

askGame :: GameInteract mode l cn r ph pl i :> es => Eff es (GameState l cn r ph pl i)
askGame = do
    GameInteract (Game gd _) <- getStaticRep
    return gd

-- liftObserve :: (GameInteract 'Modify l cn r ph pl i :> es) => Eff (GameInteract 'Observe l cn r ph pl i : es) a -> Eff es a
liftObserve :: Eff (GameInteract 'Observe l cn r ph pl i : es) a -> Eff (GameInteract 'Modify l cn r ph pl i : es) a
liftObserve eff = do
    GameInteract g <- getStaticRep
    raise $ evalStaticRep (GameInteract g) eff


unsafeProjToObserve :: Eff (GameInteract 'Modify l cn r ph pl i : es) a -> Eff (GameInteract 'Observe l cn r ph pl i : es) a
unsafeProjToObserve eff = do
    GameInteract g <- getStaticRep
    raise $ evalStaticRep (GameInteract g) eff


contraLiftObserve :: forall l cn r ph pl i es a b .(Eff (GameInteract 'Observe l cn r ph pl i : es) a -> b) -> Eff (GameInteract 'Modify l cn r ph pl i :es) a -> b
contraLiftObserve f =  f . unsafeProjToObserve

type ObserveGame l cn r ph pl i es = GameInteract Observe l cn r ph pl i :> es
type ModifyGame l cn r ph pl i es =  GameInteract Modify l cn r ph pl i :> es


data GameState l cn r ph pl i = GameState
  { players :: Set Player,
    objects :: GameObjects l cn r,
    currentPhase :: ph,
    phases :: ph -> Phase ph l cn r pl i
  }
  deriving (Generic)




-- These are the fundamental actions in a game. All the "verbs" of a game (besides the observations, e.g. "check" and "count") can be phrased in terms of these.
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

modifyGameState :: forall l cn r ph pl i es . (GameInteract Modify l cn r ph pl i :> es) => (GameState l cn r ph pl i -> GameState l cn r ph pl i) -> Eff es ()
modifyGameState f = do
    GameInteract (Game gs gr) <- getStaticRep @(GameInteract Modify l cn r ph pl i)
    putStaticRep (GameInteract (Game (f gs) gr))

modifyingGameState :: (GameInteract Modify l cn r ph pl i :> es) => ASetter (GameState l cn r ph pl i) (GameState l cn r ph pl i) a b -> (a -> b) -> Eff es ()
modifyingGameState o = modifyGameState . over o


assignGameState :: (GameInteract Modify l cn r ph pl i :> es) => ASetter (GameState l cn r ph pl i) (GameState l cn r ph pl i) a b -> b -> Eff es ()
assignGameState l b = modifyGameState (set l b)

getsGameState :: forall mode l cn r ph pl i es b . (GameInteract mode l cn r ph pl i :> es) => (GameState l cn r ph pl i -> b) -> Eff es b
getsGameState f = do
    GameInteract (Game gs _) <- getStaticRep @(GameInteract mode l cn r ph pl i)
    return (f gs)

getGameState ::  forall mode l cn r ph pl i es . (GameInteract mode l cn r ph pl i :> es) => Eff es (GameState l cn r ph pl i)
getGameState = getsGameState id

useGameState :: (GameInteract mode l cn r ph pl i :> es) => (Getting b (GameState l cn r ph pl i) b) -> Eff es b
useGameState o = getsGameState (view o)


data GameControl ph = ChangePhaseTo ph | End deriving (Eq, Ord, Show, Generic)

-- TODO: improve
continueGame :: Eff es (Maybe (GameControl ph) )
continueGame = return Nothing

showInventory :: (GameInteract Observe l cn r ph pl i :> es, Eq l, Show r, Ord r) => l -> Eff es String
showInventory l = show . inventory <$> useGameState (location l)


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

logAction' :: (Show r,
 Show l, Show cn, Show ph, Show val, Ord r, Eq l) =>
  GameAction l cn r ph -> val -> Eff '[GameInteract Modify l cn r ph pl i, Log] ()
logAction' action val =  liftObserve  (logAction action val)
-- logAction' action val = pure ()


act :: forall l r cn ph pl i es . (Ord l, Ord r, RNG :> es, ModifyGame l cn r ph pl i es,  Eq cn, Log :> es, Show ph, Show cn, Show l, Show r) => GameAction l cn r ph -> Eff es (Maybe (GameControl ph) )
act DoNothing = continueGame
act a@(MkTransfer l l' r) = (modifyingGameState (#objects . #locations) (transfer r l l'))
                            >> inject (logAction' a ' ')
                            >> continueGame
act a@(IncrementCounter c) = modifyingGameState (counter c) increment
                            >> useGameState (counter c . #val) >>= inject . logAction' a
                            >> continueGame
act a@(DecrementCounter c) = modifyingGameState (counter c) decrement
                            >> useGameState (counter c . #val) >>= inject . logAction' a
                            >> continueGame
act a@(SetCounter c v) = modifyingGameState (counter c) (`setCounter` v)
                            >> useGameState (counter c . #val) >>= inject . logAction' a
                            >> continueGame
act a@(RollCounter c) = do
  (bl, bu) <- useGameState (counter c . #bounds)
  newVal <- randomR (bl, bu)
  assignGameState (counterVal c) newVal
  _ <- useGameState (counterVal c) >>= inject . logAction' a
  continueGame
act a@(ChangePhase ph) = assignGameState #currentPhase ph
                            >> (inject (logAction' a (show ph)))
                            >> return (Just $ ChangePhaseTo ph)
act EndGame = inject (logAction' EndGame ' ') >> return (Just End)

counter :: Eq cn => cn -> Lens' (GameState l cn r ph pl i) Counter
counter c = #objects . #counters . ftAt c

counterVal :: Eq cn => cn -> Lens' (GameState l cn r ph pl i) (Cnt Int)
counterVal c = counter c . #val

location :: Eq l => l -> Lens' (GameState l cn r ph pl i) (LocationShape r)
location l = #objects . #locations . ftAt l


makeFields ''GameState
makeFields ''Game
makeFields ''Phase

mkActionNode :: GameAction l cn r ph -> GameNode l cn r ph pl i
mkActionNode action = GameNode (Left action) Nothing

mkGetOptionsNode :: Player -> Options pl i -> GameNode l cn r ph pl i
mkGetOptionsNode p choice = GameNode (Right choice) (Just p)


chooseNode :: forall l cn r ph pl i es. (Choosing :> es, GameInteract Observe l cn r ph pl i :> es, Log :> es, Show pl, Show i) =>  Eff es (Options pl i) -> [Eff es [GameNode l cn r ph pl i]]
chooseNode cs =
  let cs' = cs
      chosen =  do
        options <- cs'
        logInfo (T.pack ("Choosing from " ++ show options)) ' '
        c <- choose options
        logInfo (T.pack ("Chose " ++ show c)) ' '
        return c
   in pure . fmap concat . join . fmap sequence $ (t <*> chosen )
  where
    t = fmap (fmap (fmap inject)) $ (\(GameInteract (Game _ pr)) -> pr) <$> getStaticRep @(GameInteract Observe l cn r ph pl i) :: Eff es (pl-> [Eff es [GameNode l cn r ph pl i]])

data Choosing :: Effect where
  Choose :: GameState l cn r ph pl i -> Options pl i -> Choosing m pl

type instance DispatchOf Choosing = 'Dynamic

choose :: forall l cn r ph pl mode es i. (Choosing :> es, GameInteract mode l cn r ph pl i :> es) => Options pl i -> Eff es pl
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


chooseBasicInput :: forall pl es . (IOE :> es) => Eff (Choosing : es) pl -> Eff es pl
chooseBasicInput = interpret $ \_ -> \case
    Choose _ cs' -> do
        let cs = cs' ^. #legal . to NE.toList
        liftIO $ loopChoice cs
   where
        loopChoice cs = do
            c <- liftIO getChar
            case readMaybe [c] :: Maybe Int of
                Nothing -> putStrLn "couldn't parse" >> loopChoice cs
                Just i -> case listToMaybe (drop (i-1) cs) of
                            Just pl -> return pl
                            Nothing -> putStrLn "couldn't find" >> loopChoice cs


runNode :: forall l r cn ph pl es i. (Ord l, Ord r, Ord cn, Finitary cn, Choosing :> es, ModifyGame l cn r ph pl i es, RNG :> es, Log :> es, Show ph, Show cn, Show l, Show r, Show pl, Show i) => GameNode l cn r ph pl i -> Eff es (Either  (GameControl ph) [Eff es [GameNode l cn r ph pl i]])
runNode aNode = either handleAction handleChoice (view #node aNode)
    where
        handleAction :: GameAction l cn r ph -> Eff es (Either (GameControl ph) [Eff es [GameNode l cn r ph pl i]])
        handleAction = fmap handleActionResult . act
        handleActionResult :: Maybe (GameControl ph) -> Either (GameControl ph) [Eff es [GameNode l cn r ph pl i]]
        handleActionResult (Just a) = Left a
        handleActionResult Nothing = Right []
        handleChoice :: Options pl i -> Eff es (Either (GameControl ph) [Eff es [GameNode l cn r ph pl i]])
        handleChoice =  handleChoiceResult . fmap (inject . liftObserve) . chooseNode . unsafeProjToObserve . inject . (pure :: a -> Eff es a)
        handleChoiceResult :: a -> Eff es (Either a1 a)
        handleChoiceResult = pure . Right

newtype TreeMonad l cn r ph pl i es a = TreeMonad {unTreeMonad :: Eff es (Either (GameControl ph) [a])}
    deriving (Functor)

-- TODO: surely some nice way to do this
instance Applicative (TreeMonad l cn r ph pl i es) where
    pure x = TreeMonad (pure . pure . pure $ x)
    treefs <*> treexs = TreeMonad $ do
        efffs <- unTreeMonad treefs
        effxs <- unTreeMonad treexs
        return $ liftA2 (<*>) efffs effxs

instance Monad (TreeMonad l cn r ph pl i es) where
  -- (>>=) :: TreeMonad l cn r ph pl i es a
  --   -> (a -> TreeMonad l cn r ph pl i es b)
  --   -> TreeMonad l cn r ph pl i es b
  (TreeMonad effxs) >>= treefs = let
    efffs = unTreeMonad . treefs
                                  in TreeMonad $
    (fmap (((fmap concat . join) . fmap sequence) . sequence) . join . fmap (((traverse sequence) . sequence) . fmap (fmap efffs))) effxs


runFromSeeds2 :: forall l r cn ph pl es i. (Ord l, Ord r, Ord cn, Finitary cn, ModifyGame l cn r ph pl i es, Choosing :> es, RNG :> es, Log :> es, Show ph, Show cn, Show l, Show r, Show pl, Show i) => [Eff es [GameNode l cn r ph pl i]] ->  Eff es ()
runFromSeeds2 nodes = do
        theTree <- fmap (fmap concat) . unTreeMonad . unfoldForestM ( unfoldFunc) $ nodes
        case theTree of
          Left End -> pure ()
          Left (ChangePhaseTo ph) -> do
            phases <- useGameState #phases
            let thisPhase = phases ph
            runFromSeeds2 (inject . liftObserve <$> getPhaseNodes thisPhase)
          Right _ -> error "oops more nodes" -- TODO: make this unrepresentable!!

    
    where
    unfoldFunc :: Eff es [GameNode l cn r ph pl i]
              -> TreeMonad l cn r ph pl i es (GameNode l cn r ph pl i, [Eff es [GameNode l cn r ph pl i]])
                  -- -> Eff es
                  --      (Either
                  --         (GameControl ph)
                  --         [(GameNode l cn r ph pl i, [Eff es [GameNode l cn r ph pl i]])])
    unfoldFunc effNodes = TreeMonad $ do
        nodes <- effNodes --_ . fmap sequence . join . fmap (traverse unfoldFunc') $ effNodes
        unfolded <- traverse unfoldFunc' nodes
        let unfolded' = sequence unfolded
        return unfolded'

    unfoldFunc' :: GameNode l cn r ph pl i
                  -> Eff es
                       (Either
                          (GameControl ph)
                          (GameNode l cn r ph pl i, [Eff es [GameNode l cn r ph pl i]]))
    unfoldFunc' node = do
        result <- runNode node
        return ((node,) <$> result)

playGame ::  forall l cn r ph pl es i. (Ord l, Ord r, Ord cn, Finitary cn, RNG :> es, Choosing :> es, ModifyGame l cn r ph pl i es, Log :> es, Show ph, Show cn, Show l, Show r, Show pl, Show i) => Eff es (GameState l cn r ph pl i)
playGame = do
    gs <- getGameState
    let phases =  gs ^. #phases
    currentPhase <- useGameState #currentPhase
    let newNodes = (inject . liftObserve) <$> getPhaseNodes (phases currentPhase)
    runFromSeeds2 newNodes
    getGameState

playGivenNodes ::  forall l cn r ph pl es i. (Ord l, Ord r, Ord cn, Finitary cn, RNG :> es, Choosing :> es, ModifyGame l cn r ph pl i es, Log :> es, Show ph, Show cn, Show l, Show r, Show pl, Show i) => [Eff es [GameNode l cn r ph pl i]] -> Eff es (GameState l cn r ph pl i)
playGivenNodes nodes = do
    runFromSeeds2 nodes
    getGameState


action :: (Ord l, Ord r, Ord cn, Finitary cn, Show ph, Show cn, Show l, Show r, Show pl, Show i) => GameState l cn r ph pl i -> PlayRunner l cn r ph pl i -> IO (GameState l cn r ph pl i)
action gdata playRunner = do
  gen <- newCryptoRNGState
  withStdOutLogger $ \stdOutLogger -> runEff .  runGameInteract gdata playRunner . runCryptoRNG gen . chooseBasicInput . runLog "main" stdOutLogger defaultLogLevel $ playGame

----- Condition, perhaps soon to be Observation?
-- type Condition l cn r ph pl i es a = Eff es a

runNodesAgainstState :: (Ord l, Ord r, Ord cn, Finitary cn, Show ph, Show cn, Show l, Show r, Show pl, Show i) => GameState l cn r ph pl i -> PlayRunner l cn r ph pl i -> [GameNode l cn r ph pl i] -> IO (GameState l cn r ph pl i)
runNodesAgainstState game playRunner nodes = do
    gen <- newCryptoRNGState
    withStdOutLogger $ \stdOutLogger -> runEff . runGameInteract game playRunner . runCryptoRNG gen . chooseRandom . runLog "main" stdOutLogger defaultLogLevel $ (playGivenNodes [pure  nodes])

runEffNodesAgainstState :: (Ord l, Ord r, Ord cn, Finitary cn, Show ph, Show cn, Show l, Show r, Show pl, Show i) => GameState l cn r ph pl i -> PlayRunner l cn r ph pl i -> [Eff ('[Log, Choosing, RNG,
                    GameInteract 'Modify l cn r ph pl i, IOE]) [GameNode l cn r ph pl i]] -> IO (GameState l cn r ph pl i)
runEffNodesAgainstState game playRunner nodes = do
    gen <- newCryptoRNGState
    withStdOutLogger $ \stdOutLogger -> runEff . runGameInteract game playRunner . runCryptoRNG gen . chooseRandom . runLog "main" stdOutLogger defaultLogLevel $ (playGivenNodes nodes)





instance Num a => Num (Eff es a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = return . fromInteger
  negate = fmap negate

