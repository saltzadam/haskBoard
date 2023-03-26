{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
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
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# HLINT ignore "Use forM_" #-}
{-# HLINT ignore "Use =<<" #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# HLINT ignore "Use void" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE BangPatterns #-}

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
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import Game.Options
import Effectful.Dispatch.Static (StaticRep, SideEffects(..), getStaticRep, evalStaticRep, putStaticRep)
import GHC.Base (Applicative (..))
import Text.Read (readMaybe)
import Data.Maybe (listToMaybe)
import Data.Bitraversable
import GameNode ( GameNode, GameAction(..) )
import TreeMonad
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import GHC.IO.Handle (Handle)

-- TODO: export list

-- TODO: (lower) abstract out log
-- TODO: (fun) some kind of history besides log


data Phase phaseName l cn r playName i = Phase {
    name :: phaseName,
    seedNodes :: [Eff '[GameInteract 'Observe l cn r phaseName playName i] [GameNode l cn r phaseName playName i]]
  } deriving (Generic)

getPhaseNodes :: Phase phaseName l cn r playName i -> [Eff '[GameInteract 'Observe l cn r phaseName playName i] [GameNode l cn r phaseName playName i]]
getPhaseNodes (Phase _ seedNodes') = seedNodes'

-- thanks /u/typedbyte
-- https://www.reddit.com/r/haskell/comments/10ql43j/monthly_hask_anything_february_2023/jabrmxk/

data Mode = Observe | Modify deriving (Eq, Ord, Show, Generic)

type PlayRunner' l cn r ph pl i mode = pl -> [Eff '[GameInteract mode l cn r ph pl i] [GameNode l cn r ph pl i]]

type PlayRunner l cn r ph pl i = PlayRunner' l cn r ph pl i 'Observe

data Game l cn r ph pl i = Game { gameState :: GameState l cn r ph pl i,
                                  playRunner :: PlayRunner l cn r ph pl i
                                } deriving (Generic)


data GameInteract (mode :: Mode) l cn r ph pl i :: Effect
type instance DispatchOf (GameInteract mode l cn r ph pl i) = 'Static 'NoSideEffects
newtype instance StaticRep (GameInteract mode l cn r ph pl i) = GameInteract (Game l cn r ph pl i)

liftStaticRepMode :: StaticRep (GameInteract 'Observe l cn r ph pl i) -> StaticRep (GameInteract 'Modify l cn r ph pl i)
liftStaticRepMode (GameInteract g) = GameInteract g


maybeUnsafeUnliftStaticRepMode :: StaticRep (GameInteract 'Modify l cn r ph pl i) -> StaticRep (GameInteract 'Observe l cn r ph pl i)
maybeUnsafeUnliftStaticRepMode (GameInteract g) = GameInteract g

runGameInteract :: forall l cn r ph pl i mode es a. GameState l cn r ph pl i
                -> PlayRunner l cn r ph pl i
                -> Eff (GameInteract mode l cn r ph pl i : es) a -> Eff es a
runGameInteract gd pr = evalStaticRep (GameInteract (Game gd pr) :: StaticRep (GameInteract mode l cn r ph pl i))

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

type ObserveGame l cn r ph pl i es = GameInteract 'Observe l cn r ph pl i :> es
type ModifyGame l cn r ph pl i es =  GameInteract 'Modify l cn r ph pl i :> es

data GameState l cn r ph pl i = GameState
  { players :: Set Player,
    objects :: GameObjects l cn r,
    currentPhase :: ph,
    phases :: ph -> Phase ph l cn r pl i
  }
  deriving (Generic)




askRunner :: forall l cn r ph pl i es . (GameInteract 'Observe l cn r ph pl i :> es) => Eff es (PlayRunner l cn r ph pl i)
askRunner = do
    GameInteract (Game _ pr) <- getStaticRep @(GameInteract 'Observe l cn r ph pl i)
    return pr

getRunner  :: forall l cn r ph pl i es . (GameInteract 'Modify l cn r ph pl i :> es) => Eff es (PlayRunner l cn r ph pl i)
getRunner = do
    GameInteract (Game _ pr) <- getStaticRep @(GameInteract 'Modify l cn r ph pl i)
    return pr


modifyGameState :: forall l cn r ph pl i es . (GameInteract 'Modify l cn r ph pl i :> es) => (GameState l cn r ph pl i -> GameState l cn r ph pl i) -> Eff es ()
modifyGameState f = do
    GameInteract (Game gs gr) <- getStaticRep @(GameInteract 'Modify l cn r ph pl i)
    putStaticRep (GameInteract (Game (f gs) gr))

modifyingGameState :: (GameInteract 'Modify l cn r ph pl i :> es) => ASetter (GameState l cn r ph pl i) (GameState l cn r ph pl i) a b -> (a -> b) -> Eff es ()
modifyingGameState o = modifyGameState . over o


assignGameState :: (GameInteract 'Modify l cn r ph pl i :> es) => ASetter (GameState l cn r ph pl i) (GameState l cn r ph pl i) a b -> b -> Eff es ()
assignGameState l b = modifyGameState (set l b)

getsGameState :: forall mode l cn r ph pl i es b . (GameInteract mode l cn r ph pl i :> es) => (GameState l cn r ph pl i -> b) -> Eff es b
getsGameState f = do
    GameInteract (Game gs _) <- getStaticRep @(GameInteract mode l cn r ph pl i)
    return (f gs)

getGameState ::  forall mode l cn r ph pl i es . (GameInteract mode l cn r ph pl i :> es) => Eff es (GameState l cn r ph pl i)
getGameState = getsGameState id

useGameState :: (GameInteract mode l cn r ph pl i :> es) => Getting b (GameState l cn r ph pl i) b -> Eff es b
useGameState o = getsGameState (view o)

observe :: Game l cn r ph pl i -> Eff '[GameInteract mode l cn r ph pl i] a -> a
observe g eff = runPureEff . runGameInteract (g ^. #gameState) (g^. #playRunner) $ eff

showInventory :: (GameInteract 'Observe l cn r ph pl i :> es, Eq l, Show r, Ord r) => l -> Eff es String
showInventory l = show . inventory <$> useGameState (location l)


data Log2 :: Effect where
    LogThis :: Text -> Log2 m Text
type instance DispatchOf Log2 = 'Dynamic

logThis' :: (Log2 :> es) => Text -> Eff es Text
logThis' text = send (LogThis text)

logThis ::  (Log2 :> es) => Text -> Eff es ()
logThis text = logThis' text >> return ()

logStdOut :: (IOE :> es) =>
    Eff (Log2 : es) a ->
    Eff es a
logStdOut = interpret $ \_ -> \case
    LogThis loggable -> liftIO (TIO.putStrLn loggable) >> return loggable

logToFile :: (IOE :> es) =>
    Handle ->
    Eff (Log2 : es) a ->
    Eff es a
logToFile handle = interpret $ \_ -> \case
    LogThis loggable -> liftIO (TIO.hPutStrLn handle loggable) >> return loggable

logAction2 :: (ObserveGame l cn r ph pl i es, Log2 :> es, Show cn, Show ph, Show val, Ord r, Eq l, Show r, Show l) => GameAction l cn r ph -> val -> Eff es ()
logAction2 (IncrementCounter cn) val = logThis (T.pack $ "Incremented " ++ show cn ++ " to " ++ show val)
logAction2 (DecrementCounter cn) val =  logThis (T.pack $ "Decremented " ++ show cn ++ " to " ++ show val)
logAction2 (SetCounter cn i) _ = logThis (T.pack $ "Set " ++ show cn ++ " to " ++ show i)
logAction2 (RollCounter cn) val = logThis (T.pack $ "Rolled " ++ show cn ++ " to " ++ show val)
logAction2 (ChangePhase ph) _ = logThis (T.pack $ "Changed phase to " ++ show ph)
logAction2 DoNothing _ = pure ()
logAction2 EndGame _ = logThis "Ended game"
logAction2 (MkTransfer l l' r) _ = do
    invl <- showInventory l
    invl' <- showInventory l'
    logThis (T.pack $ "Transfered " ++ show r ++ " from " ++ show l ++ " to " ++ show l'
                                   ++ "\n Contents of " ++ show l ++ ": " ++ invl
                                   ++ "\n Contents of " ++ show l' ++ ": " ++ invl')

logAction2' :: (Log2 :> es, Ord r, Show cn, Show ph, Show val, Show r, Show l,
 Eq l) => GameAction l cn r ph -> val -> Eff (GameInteract 'Modify l cn r ph pl i : es) ()
logAction2' action val = liftObserve (logAction2 action val)


act :: forall l r cn ph pl i es . (Ord l, Ord r, RNG :> es, ModifyGame l cn r ph pl i es,  Eq cn,  Show ph, Show cn, Show l, Show r, Log2 :> es) => GameAction l cn r ph -> Eff es (Maybe (GameControl ph) )
act DoNothing = continueGame
act a@(MkTransfer l l' r) = modifyingGameState (#objects . #locations) (transfer r l l')
                            >> inject (logAction2' a ' ')
                            >> continueGame
act a@(IncrementCounter c) = modifyingGameState (counter c) increment
                            >> useGameState (counter c . #val) >>= inject . logAction2' a
                            >> continueGame
act a@(DecrementCounter c) = modifyingGameState (counter c) decrement
                            >> useGameState (counter c . #val) >>= inject . logAction2' a
                            >> continueGame
act a@(SetCounter c v) = modifyingGameState (counter c) (`setCounter` v)
                            >> useGameState (counter c . #val) >>= inject . logAction2' a
                            >> continueGame
act a@(RollCounter c) = do
  (bl, bu) <- useGameState (counter c . #bounds)
  newVal <- randomR (bl, bu)
  assignGameState (counterVal c) newVal
  _ <- useGameState (counterVal c) >>= inject . logAction2' a
  continueGame
act a@(ChangePhase ph) = assignGameState #currentPhase ph
                            >> inject (logAction2' a (show ph))
                            >> return (Just $ ChangePhaseTo ph)
act EndGame = inject (logAction2' EndGame ' ') >> return (Just End)

counter :: Eq cn => cn -> Lens' (GameState l cn r ph pl i) Counter
counter c = #objects . #counters . ftAt c

counterVal :: Eq cn => cn -> Lens' (GameState l cn r ph pl i) (Cnt Int)
counterVal c = counter c . #val

location :: Eq l => l -> Lens' (GameState l cn r ph pl i) (LocationShape r)
location l = #objects . #locations . ftAt l


makeFields ''GameState
makeFields ''Game
makeFields ''Phase


chooseNode :: forall l cn r ph pl i es. (Choosing :> es, GameInteract 'Observe l cn r ph pl i :> es,  Show pl, Show i, Show l, Show r, Show cn, Show ph, Log2 :> es) =>  Eff es (Options pl i) -> Eff es [Eff '[GameInteract 'Observe l cn r ph pl i] [GameNode l cn r ph pl i]]
chooseNode cs =
  let cs' = cs
  in
     askRunner <*> (do
        options <- cs'
        logThis (T.pack ("Choosing from " ++ show options))
        c <- choose options
        logThis (T.pack ("Chose " ++ show c))
        return c)

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


runNode :: forall l r cn ph pl es i. (Ord l, Ord r, Ord cn, Finitary cn, Choosing :> es, ModifyGame l cn r ph pl i es, RNG :> es,  Show ph, Show cn, Show l, Show r, Show pl, Show i, Log2 :> es) => GameNode l cn r ph pl i -> Eff es (Either  (GameControl ph) [Eff es [GameNode l cn r ph pl i]])
runNode aNode = maybeLeftToEmptyRight <$> bitraverse handleAction handleChoice (view #node aNode)
    where
        handleAction :: GameAction l cn r ph -> Eff es (Maybe (GameControl ph))
        handleAction = inject . act

        handleChoice :: Options pl i -> Eff es [ Eff es [GameNode l cn r ph pl i]]
        handleChoice = fmap (fmap (inject . liftObserve)) . inject . liftObserve . chooseNode . inject. unsafeProjToObserve . pure

        maybeLeftToEmptyRight :: Monoid b => Either (Maybe a) b -> Either a b
        maybeLeftToEmptyRight (Left Nothing) = Right mempty
        maybeLeftToEmptyRight (Left (Just i)) = Left i
        maybeLeftToEmptyRight (Right x) = Right x

runFromSeeds2 :: forall l r cn ph pl es i. (Ord l, Ord r, Ord cn, Finitary cn, ModifyGame l cn r ph pl i es, Choosing :> es, RNG :> es,  Show ph, Show cn, Show l, Show r, Show pl, Show i, Log2 :> es) => [Eff es [GameNode l cn r ph pl i]] ->  Eff es ()
runFromSeeds2 nodes = do
        theTree <- fmap (fmap concat) . unTreeMonad . unfoldForestM unfoldFunc $ nodes
        case theTree of
          Left End -> pure ()
          Left (ChangePhaseTo ph) -> do
            phaser <- useGameState #phases
            let thisPhase = phaser ph
            runFromSeeds2 (inject . liftObserve <$> getPhaseNodes thisPhase)
          Right _ -> error "oops more nodes" -- TODO: make this unrepresentable!!


    where
    unfoldFunc :: Eff es [GameNode l cn r ph pl i]
              -> TreeMonad l cn r ph pl i es (GameNode l cn r ph pl i, [Eff es [GameNode l cn r ph pl i]])
    unfoldFunc effNodes = TreeMonad $ do
        nodes' <- effNodes
        unfolded <- traverse unfoldFunc' nodes'
        let unfolded' = sequence unfolded
        return unfolded'

    unfoldFunc' :: GameNode l cn r ph pl i
                  -> Eff es
                       (Either
                          (GameControl ph)
                          (GameNode l cn r ph pl i, [Eff es [GameNode l cn r ph pl i]]))
    unfoldFunc' aNode = do
        result <- runNode aNode
        return ((aNode,) <$> result)

playGame ::  forall l cn r ph pl es i. (Ord l, Ord r, Ord cn, Finitary cn, RNG :> es, Choosing :> es, ModifyGame l cn r ph pl i es,  Show ph, Show cn, Show l, Show r, Show pl, Show i, Log2 :> es) => Eff es (GameState l cn r ph pl i)
playGame = do
    gs <- getGameState
    let phases =  gs ^. #phases
    currentPhase <- useGameState #currentPhase
    let newNodes = inject . liftObserve <$> getPhaseNodes (phases currentPhase)
    runFromSeeds2 newNodes
    getGameState

playGivenNodes ::  forall l cn r ph pl es i. (Ord l, Ord r, Ord cn, Finitary cn, RNG :> es, Choosing :> es, ModifyGame l cn r ph pl i es,  Show ph, Show cn, Show l, Show r, Show pl, Show i, Log2 :> es) => [Eff es [GameNode l cn r ph pl i]] -> Eff es (GameState l cn r ph pl i)
playGivenNodes nodes = do
    runFromSeeds2 nodes
    getGameState


action :: (Ord l, Ord r, Ord cn, Finitary cn, Show ph, Show cn, Show l, Show r, Show pl, Show i) => GameState l cn r ph pl i -> PlayRunner l cn r ph pl i -> IO (GameState l cn r ph pl i)
action gdata playRunner = do
  gen <- newCryptoRNGState
  runEff .  runGameInteract gdata playRunner . runCryptoRNG gen . chooseBasicInput . logStdOut $ playGame

----- Condition, perhaps soon to be Observation?
-- type Condition l cn r ph pl i es a = Eff es a

runNodesAgainstState :: (Ord l, Ord r, Ord cn, Finitary cn, Show ph, Show cn, Show l, Show r, Show pl, Show i) => GameState l cn r ph pl i -> PlayRunner l cn r ph pl i -> [GameNode l cn r ph pl i] -> IO (GameState l cn r ph pl i)
runNodesAgainstState game playRunner nodes = do
    gen <- newCryptoRNGState
    runEff . runGameInteract game playRunner . runCryptoRNG gen . chooseRandom . logStdOut $ playGivenNodes [pure  nodes]

runEffNodesAgainstState :: (Ord l, Ord r, Ord cn, Finitary cn, Show ph, Show cn, Show l, Show r, Show pl, Show i) => GameState l cn r ph pl i -> PlayRunner l cn r ph pl i -> [Eff '[Log2, Choosing, RNG,
                    GameInteract 'Modify l cn r ph pl i, IOE] [GameNode l cn r ph pl i]] -> IO (GameState l cn r ph pl i)
runEffNodesAgainstState game playRunner nodes = do
    gen <- newCryptoRNGState
    runEff . runGameInteract game playRunner . runCryptoRNG gen . chooseRandom . logStdOut $ playGivenNodes nodes





instance Num a => Num (Eff es a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = return . fromInteger
  negate = fmap negate

