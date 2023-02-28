{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE NoFieldSelectors #-}
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
{-# LANGUAGE MultiParamTypeClasses #-}

module GameE where

import Control.Lens (Getting, makeFields, over, view, (^.))
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
import Effectful.Reader.Static (Reader)
import qualified Effectful.Reader.Static as R
import qualified Effectful.State.Static.Local as S
import FinitaryMap (ftAt)
import GHC.Generics
-- import Game (Phase)
-- import Game (Phase)
import Location (GameObjects (..), decrement, increment, setCounter, transfer, inventory)
import Data.Finitary (Finitary)
import Data.Bitraversable (bitraverse)
import Data.Tree (Tree (..))
import Data.Set (Set)
import Game.Player (Player)
import Control.Applicative
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad (MonadPlus(..))
import Effectful.Log (Log, logInfo, runLog, defaultLogLevel)
import qualified Data.Text as T
import Log.Backend.StandardOutput

-- import Control.Lens (modifying)

data GameNode l cn r ph pl = GameNode
  { node :: Either  (GameAction l cn r ph) (Choice l cn r ph pl),
    owner :: Maybe Player
  }
  deriving (Generic)

-- instance KnownPrefix 
-- subset es' (e:es')

-- instance Subset es (e : es)

observeGame ::  (Subset es' es, S.State (GameData l cn r ph) :> es) =>  Eff (GameDataR l cn r ph:es') a -> Eff es a
observeGame eff = S.stateM (\r -> inject $ fmap (,r) (R.runReader r eff))



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


data Phase phaseName l cn r playName = Phase {
    name :: phaseName,
    -- seedNodes :: ObserveGame l cn r phaseName [GameNode l cn r phaseName playName ]
    seedNodes :: ObserveGame l cn r phaseName [GameNode l cn r phaseName playName]
  } deriving (Generic)



-- TODO: is there a clean way to restrict state to reader?

type GameDataS l cn r ph = S.State (GameData l cn r ph)

type GameDataR l cn r ph = R.Reader (GameData l cn r ph)

type GameRulesR l cn r ph pl = R.Reader (GameRules l cn r ph pl)

type ObserveGame l cn r ph = Eff '[GameDataR l cn r ph, Log]
type ObserveRulesGame l cn r ph pl = Eff '[ GameRulesR l cn r ph pl, GameDataR l cn r ph, Log]
type ModifyGame l cn r ph = Eff '[GameDataS l cn r ph, Log]

data GameData l cn r ph = GameData
  { players :: Set Player,
    objects :: GameObjects l cn r,
    currentPhase :: ph
  }
  deriving (Generic)

data GameRules l cn r ph pl = GameRules
  { phases :: ph -> Phase ph l cn r pl,
    runPlay :: pl -> Eff '[GameRulesR l cn r ph pl, GameDataR l cn r ph, Log] [GameNode l cn r ph pl]
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

modifyWithPass :: (S.State t :> es) => (t -> t) -> Eff es t
modifyWithPass f = S.state $ \s -> (s, f s)

modifyingWithPass :: (S.State t :> es) => ASetter t t a b -> (a -> b) -> Eff es t
modifyingWithPass o = modifyWithPass . over o

-- modifying :: (S.State s :> es) => ASetter s s a b -> (a -> b) -> Eff es ()
modifying :: (S.State s :> es) => ASetter s s a b -> (a -> b) -> Eff es ()
modifying o = S.modify . over o
{-# INLINE modifying #-}

assignWithPass :: (S.State t :> es) => ASetter t t a b -> b -> Eff es t
assignWithPass o = modifyingWithPass o . const

assign :: (S.State s :> es) => ASetter s s a b -> b -> Eff es ()
assign o = modifying o . const

use :: (S.State s :> es) => Getting a s a -> Eff es a
use o = S.gets (view o)

data (GameControl ph) = Continue | ChangePhaseTo ph | End deriving (Eq, Ord, Show, Generic)

continueGame :: Eff es (GameControl ph)
continueGame = return Continue

showInventory :: (GameDataR l cn r ph :> es, Eq l, Show r, Ord r) => l -> Eff es String
showInventory l = do 
    gd <- R.ask
    let l_loc = view (#objects . #locations . ftAt l) gd
    return . show $ inventory l_loc
    

logAction :: (Show r, Show l, Show cn, Show ph, Show val, Ord r, Eq l) => GameAction l cn r ph -> val -> Eff [GameDataR l cn r ph, Log] ()
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


act' :: (Ord l, Ord r, RNG :> es, S.State (GameData l cn r ph) :> es,  Eq cn, Log :> es, Show ph, Show cn, Show l, Show r) => GameAction l cn r ph -> Eff es (GameControl ph)
act' DoNothing = continueGame
act' a@(MkTransfer l l' r) = modifyingWithPass (#objects . #locations) (transfer r l l')
                            >> observeGame (logAction a ' ')
                            >> continueGame
act' a@(IncrementCounter c) = modifyingWithPass (#objects . #counters . ftAt c) increment
                            >> (S.gets (view (#objects . #counters . ftAt c . #val)) >>= observeGame . logAction a)
                            >> continueGame
act' a@(DecrementCounter c) = modifyingWithPass (#objects . #counters . ftAt c) decrement
                            >> (S.gets (view (#objects . #counters . ftAt c . #val)) >>= observeGame . logAction a)
                            >> continueGame
act' a@(SetCounter c v) = modifyingWithPass (#objects . #counters . ftAt c) (`setCounter` v)
                            >> (S.gets (view (#objects . #counters . ftAt c . #val)) >>= observeGame . logAction a)
                            >> continueGame
act' a@(RollCounter c) = do
  (bl, bu) <- S.gets (view (#objects . #counters . ftAt c . #bounds))
  newVal <- randomR (bl, bu)
  assign (#objects . #counters . ftAt c . #val) newVal
  S.gets (view (#objects . #counters . ftAt c . #val)) >>= observeGame . logAction a
  continueGame
act' a@(ChangePhase ph) = assignWithPass #currentPhase ph
                            >> observeGame (logAction a (show ph))
                            >> return (ChangePhaseTo ph)
act' EndGame = return End

act :: (Ord l, Ord r, S.State (GameData l cn r ph) :> es, RNG :> es, Log :> es, Show r, Show l, Show cn, Show ph, Eq cn) => GameAction l cn r ph -> Eff es (GameControl ph)
act = act'

-- The flow of a game looks like this: there is some sequence of `GameActions` (draw a card, advance the turn counter) until a player must make a `Choice`. Choices produce sequences of actions and additional choices, and so on.

type Choice l cn r ph pl = Eff '[GameDataR l cn r ph, Log] [pl]

makeFields ''GameRules
makeFields ''GameData

mkActionNode :: GameAction l cn r ph -> GameNode l cn r ph pl
mkActionNode action = GameNode (Left action) Nothing

mkChoiceNode :: Player -> Choice l cn r ph pl -> GameNode l cn r ph pl
mkChoiceNode p choice = GameNode (Right choice) (Just p)



-- TODO: make Choice Nonempty -- should never send an empty list!
chooseNode ::  Show pl =>  Choice l cn r ph pl
        -> Eff '[GameDataR l cn r ph, Choosing, GameRulesR l cn r ph pl, Log] [GameNode l cn r ph pl]
chooseNode cs =
  let cs' = inject cs
   in do
        options <- cs'
        logInfo (T.pack ("Choosing from " ++ show options)) ' '
        playRunner <- R.asks (view #runPlay :: GameRules l cn r ph pl -> (pl -> Eff '[GameRulesR l cn r ph pl, GameDataR l cn r ph, Log] [GameNode l cn r ph pl]))
        c <- chooseS options
        logInfo (T.pack ("Chose " ++ show c)) ' '
        inject $ playRunner c

data Choosing :: Effect where
  Choose :: GameData l cn r ph -> [pl] -> Choosing m pl

type instance DispatchOf Choosing = 'Dynamic

choose :: (Choosing :> es) => GameData l cn r ph -> [pl] -> Eff es pl
choose g cs = send (Choose g cs)

chooseS :: forall l cn r ph pl es. (Choosing :> es, Reader (GameData l cn r ph) :> es) => [pl] -> Eff es pl
chooseS cs = R.ask @(GameData l cn r ph) >>= \g -> send (Choose g cs)

chooseFirst :: Eff (Choosing : es) pl -> Eff es pl
chooseFirst = interpret $ \_ -> \case
  Choose _ cs -> return $ head cs

chooseRandom :: (RNG :> es) => Eff (Choosing : es) pl -> Eff es pl
chooseRandom = interpret $ \_ -> \case
  Choose _ cs ->
    let choice = randomR (0, length cs - 1)
     in fmap (cs !!) choice


runNode :: forall l r cn ph pl es . (Ord l, Ord r, Ord cn, Finitary cn, GameRulesR l cn r ph pl :> es, Choosing :> es, S.State (GameData l cn r ph) :> es, RNG :> es, Log :> es, Show ph, Show cn, Show l, Show r, Show pl, GameRulesR l cn r ph pl :> es) => GameNode l cn r ph pl -> Eff es (Either  (GameControl ph) [GameNode l cn r ph pl])
runNode aNode = bitraverse act (observeGame . chooseNode :: Choice l cn r ph pl -> Eff es [GameNode l cn r ph pl] ) $ view #node aNode

data TreeControl b = TContinue | TStop | TRestart [b]

unfoldTreeControl :: (Monad m, MonadPlus m) => (b -> m (a, [b], TreeControl b)) -> b -> m (Tree a)
unfoldTreeControl f b = do
    (a, bs, c) <-  f b
    case  c of
        TContinue -> do
            ts <- unfoldForestControl f bs
            return (Node a ts)
        TStop -> mzero -- will stop entire computation
        TRestart newbs ->  mzero <|>  do
            ts <- unfoldForestControl f newbs
            return (Node a ts)


unfoldForestControl :: (Monad m, MonadPlus m) => (b -> m (a, [b], TreeControl b)) -> [b] -> m [Tree a]
unfoldForestControl f =  mapM (unfoldTreeControl f)


runFromSeeds :: forall l r cn ph pl es . (Ord l, Ord r, Ord cn, Finitary cn, GameRulesR l cn r ph pl :> es, S.State (GameData l cn r ph) :> es, Choosing :> es, RNG :> es, Log :> es, Show ph, Show cn, Show l, Show r, Show pl) => [GameNode l cn r ph pl] -> MaybeT (Eff es) [Tree (GameNode l cn r ph pl)]
runFromSeeds = unfoldForestControl treefunc
    where
        treefunc :: (GameRulesR l cn r ph pl :> es) => GameNode l cn r ph pl -> MaybeT (Eff es) (GameNode l cn r ph pl, [GameNode l cn r ph pl], TreeControl (GameNode l cn r ph pl))
        treefunc nod = MaybeT $ do
                    result <-  runNode  nod
                    case result of
                      Left Continue ->  return $ Just (nod, [], TContinue)
                      Left End ->  return $ Just (nod, [], TStop)
                      Left (ChangePhaseTo ph) -> do
                            phases <- R.asks (view #phases :: GameRules l cn r ph pl -> (ph -> Phase ph l cn r pl))
                            newNodes <- observeGame $ view #seedNodes (phases ph)
                            return $ Just (nod, [], TRestart newNodes)
                      Right moreNodes -> return $ Just (nod, moreNodes, TContinue)

playGame ::  forall l cn r ph pl es . ( GameRulesR l cn r ph pl :> es, Ord l, Ord r, Ord cn, Finitary cn, RNG :> es, Choosing :> es, S.State (GameData l cn r ph) :> es, Log :> es, Show ph, Show cn, Show l, Show r, Show pl) => Eff es (Maybe [Tree (GameNode l cn r ph pl)])
playGame = do
    rules <- R.ask
    let phases = view #phases rules
    game <- observeGame (R.ask :: Eff '[GameDataR l cn r ph] (GameData l cn r ph))
    let currentPhase = view #currentPhase game
    newNodes <- observeGame (phases currentPhase ^. #seedNodes :: Eff '[GameDataR l cn r ph, Log] [GameNode l cn r ph pl])
    runMaybeT . runFromSeeds $ newNodes


action :: (Ord l, Ord r, Ord cn, Finitary cn, Show ph, Show cn, Show l, Show r, Show pl) => GameData l cn r ph -> GameRules l cn r ph pl -> IO (GameData l cn r ph)
action gdata grules = do
  gen <- newCryptoRNGState
  withStdOutLogger $ \stdOutLogger -> runEff .  R.runReader grules . runCryptoRNG gen . chooseRandom . runLog "main" stdOutLogger defaultLogLevel . S.execState gdata $ playGame

----- Condition, perhaps soon to be Observation?
type Condition l cn r ph a = ObserveGame l cn r ph a

-- newtype Condition l cn r ph pl a = Condition {runCondition :: ObserveGame l cn r ph a}
--     deriving (Generic)
--     deriving (Functor, Applicative, Monad) via ObserveGame l cn r ph

runNodesAgainstState :: (Ord l, Ord r, Ord cn, Finitary cn, Show ph, Show cn, Show l, Show r, Show pl) => GameData l cn r ph -> GameRules l cn r ph pl -> [GameNode l cn r ph pl] -> IO (GameData l cn r ph)
runNodesAgainstState gd grules nodes = do
    gen <- newCryptoRNGState
    withStdOutLogger $ \stdOutLogger -> runEff . R.runReader grules . runCryptoRNG gen . chooseRandom . runLog "main" stdOutLogger defaultLogLevel . S.execState gd $ (runMaybeT . runFromSeeds $ nodes)

instance Num a => Num (ObserveGame l cn r ph a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = return . fromInteger
  negate = fmap negate

-- getCondition ::  Condition l cn r ph pl a  -> ObserveGame l cn r ph a
-- getCondition = view #runCondition


