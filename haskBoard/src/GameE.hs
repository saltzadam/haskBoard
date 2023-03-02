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
{-# LANGUAGE MultiParamTypeClasses #-}

module GameE where

import Control.Lens ( Getting, makeFields, over, view, (^.), to )
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
import GHC.Generics (Generic)
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
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

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
    seedNodes :: ObserveGame l cn r phaseName i [GameNode l cn r phaseName playName i]
  } deriving (Generic)



-- TODO: is there a clean way to restrict state to reader?

type GameDataS l cn r ph = S.State (GameData l cn r ph)

type GameDataR l cn r ph = R.Reader (GameData l cn r ph)

type GameRulesR l cn r ph pl i = R.Reader (GameRules l cn r ph pl i)

type ObserveGame l cn r ph i = Eff '[GameDataR l cn r ph, Log]
type ObserveRulesGame l cn r ph pl i = Eff '[ GameRulesR l cn r ph pl i, GameDataR l cn r ph, Log]
type ModifyGame l cn r ph = Eff '[GameDataS l cn r ph, Log]

observeGame ::  (Subset es' es, GameDataS l cn r ph :> es) =>  Eff (GameDataR l cn r ph:es') a -> Eff es a
observeGame eff = S.stateM (\r -> inject $ fmap (,r) (R.runReader r eff))



data GameData l cn r ph = GameData
  { players :: Set Player,
    objects :: GameObjects l cn r,
    currentPhase :: ph
  }
  deriving (Generic)

data GameRules l cn r ph pl i = GameRules
  { phases :: ph -> Phase ph l cn r pl i,
    runPlay :: pl -> Eff '[GameRulesR l cn r ph pl i, GameDataR l cn r ph, Log] [GameNode l cn r ph pl i]
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


act' :: (Ord l, Ord r, RNG :> es, GameDataS l cn r ph :> es,  Eq cn, Log :> es, Show ph, Show cn, Show l, Show r) => GameAction l cn r ph -> Eff es (GameControl ph)
act' DoNothing = continueGame
act' a@(MkTransfer l l' r) = modifying (#objects . #locations) (transfer r l l')
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
act' EndGame = observeGame (logAction EndGame ' ') >> return End

act :: (Ord l, Ord r, GameDataS l cn r ph :> es, RNG :> es, Log :> es, Show r, Show l, Show cn, Show ph, Eq cn) => GameAction l cn r ph -> Eff es (GameControl ph)
act = act'

-- The flow of a game looks like this: there is some sequence of `GameActions` (draw a card, advance the turn counter) until a player must make a `GetOptions`. GetOptionss produce sequences of actions and additional choices, and so on.

data Legality illegal = Legal | Illegal illegal deriving (Eq, Ord, Show, Generic)

instance Semigroup (Legality i) where
    Legal <> x = x
    x <> Legal = x
    x <> _ = x

instance Monoid (Legality i) where
    mempty = Legal

data Options pl i = Options {legal :: NonEmpty pl,
                             illegal :: [(pl, Legality i)]
                            } deriving (Eq, Ord, Show, Generic)

type GetOptions l cn r ph pl i = Eff '[GameDataR l cn r ph, Log] (Options pl i)


makeFields ''Options
makeFields ''GameRules
makeFields ''GameData

mkActionNode :: GameAction l cn r ph -> GameNode l cn r ph pl i
mkActionNode action = GameNode (Left action) Nothing

mkGetOptionsNode :: Player -> GetOptions l cn r ph pl i -> GameNode l cn r ph pl i
mkGetOptionsNode p choice = GameNode (Right choice) (Just p)



-- TODO: make GetOptions Nonempty -- should never send an empty list!
chooseNode ::  (Show pl, Show i) =>  GetOptions l cn r ph pl i
        -> Eff '[GameDataR l cn r ph, Choosing, GameRulesR l cn r ph pl i, Log] [GameNode l cn r ph pl i]
chooseNode cs =
  let cs' = inject cs
   in do
        options <- cs'
        logInfo (T.pack ("Choosing from " ++ show options)) ' '
        playRunner <- R.asks (view #runPlay :: GameRules l cn r ph pl i -> (pl -> Eff '[GameRulesR l cn r ph pl i, GameDataR l cn r ph, Log] [GameNode l cn r ph pl i]))
        c <- chooseS options
        logInfo (T.pack ("Chose " ++ show c)) ' '
        inject $ playRunner c

data Choosing :: Effect where
  Choose :: GameData l cn r ph -> Options pl i -> Choosing m pl

type instance DispatchOf Choosing = 'Dynamic

choose :: (Choosing :> es) => GameData l cn r ph -> Options pl i  -> Eff es pl
choose g cs = send (Choose g cs)

chooseS :: forall l cn r ph pl es i. (Choosing :> es, Reader (GameData l cn r ph) :> es) => Options pl i -> Eff es pl
chooseS cs = R.ask @(GameData l cn r ph) >>= \g -> send (Choose g cs)

chooseFirst :: forall es pl . Eff (Choosing : es) pl -> Eff es pl
chooseFirst = interpret $ \_ -> \case
  Choose _ cs -> return (cs ^. #legal . to NE.head)

chooseRandom :: (RNG :> es) => Eff (Choosing : es) pl -> Eff es pl
chooseRandom = interpret $ \_ -> \case
  Choose _ cs' ->
    let cs = cs' ^. #legal
        choice = randomR (0, length cs - 1)
     in fmap (cs NE.!!) choice


runNode :: forall l r cn ph pl es i. (Ord l, Ord r, Ord cn, Finitary cn, GameRulesR l cn r ph pl i :> es, Choosing :> es, GameDataS l cn r ph :> es, RNG :> es, Log :> es, Show ph, Show cn, Show l, Show r, Show pl, GameRulesR l cn r ph pl i :> es, Show i) => GameNode l cn r ph pl i -> Eff es (Either  (GameControl ph) [GameNode l cn r ph pl i])
runNode aNode = bitraverse act (observeGame . chooseNode :: GetOptions l cn r ph pl i -> Eff es [GameNode l cn r ph pl i] ) $ view #node aNode

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


runFromSeeds :: forall l r cn ph pl es i. (Ord l, Ord r, Ord cn, Finitary cn, GameRulesR l cn r ph pl i :> es, GameDataS l cn r ph :> es, Choosing :> es, RNG :> es, Log :> es, Show ph, Show cn, Show l, Show r, Show pl, Show i) => [GameNode l cn r ph pl i] -> MaybeT (Eff es) [Tree (GameNode l cn r ph pl i)]
runFromSeeds = unfoldForestControl treefunc
    where
        treefunc :: (GameRulesR l cn r ph pl i :> es) => GameNode l cn r ph pl i -> MaybeT (Eff es) (GameNode l cn r ph pl i, [GameNode l cn r ph pl i], TreeControl (GameNode l cn r ph pl i))
        treefunc nod = MaybeT $ do
                    result <- runNode nod
                    case result of
                      Left Continue ->  return $ Just (nod, [], TContinue)
                      Left End -> return Nothing
                      Left (ChangePhaseTo ph) -> do
                            phases <- R.asks (view #phases :: GameRules l cn r ph pl i -> (ph -> Phase ph l cn r pl i))
                            newNodes <- observeGame $ view #seedNodes (phases ph)
                            return $ Just (nod, [], TRestart newNodes)
                      Right moreNodes -> return $ Just (nod, moreNodes, TContinue)

playGame ::  forall l cn r ph pl es i. ( GameRulesR l cn r ph pl i :> es, Ord l, Ord r, Ord cn, Finitary cn, RNG :> es, Choosing :> es, GameDataS l cn r ph :> es, Log :> es, Show ph, Show cn, Show l, Show r, Show pl, Show i) => Eff es (Maybe [Tree (GameNode l cn r ph pl i)])
playGame = do
    rules <- R.ask
    let phases = view #phases rules
    game <- observeGame (R.ask :: Eff '[GameDataR l cn r ph] (GameData l cn r ph))
    let currentPhase = view #currentPhase game
    newNodes <- observeGame (phases currentPhase ^. #seedNodes :: Eff '[GameDataR l cn r ph, Log] [GameNode l cn r ph pl i])
    runMaybeT . runFromSeeds $ newNodes


action :: (Ord l, Ord r, Ord cn, Finitary cn, Show ph, Show cn, Show l, Show r, Show pl, Show i) => GameData l cn r ph -> GameRules l cn r ph pl i -> IO (GameData l cn r ph)
action gdata grules = do
  gen <- newCryptoRNGState
  withStdOutLogger $ \stdOutLogger -> runEff .  R.runReader grules . runCryptoRNG gen . chooseRandom . runLog "main" stdOutLogger defaultLogLevel . S.execState gdata $ playGame

----- Condition, perhaps soon to be Observation?
type Condition l cn r ph i a = ObserveGame l cn r ph i a

-- newtype Condition l cn r ph pl a = Condition {runCondition :: ObserveGame l cn r ph i a}
--     deriving (Generic)
--     deriving (Functor, Applicative, Monad) via ObserveGame l cn r ph i

runNodesAgainstState :: (Ord l, Ord r, Ord cn, Finitary cn, Show ph, Show cn, Show l, Show r, Show pl, Show i) => GameData l cn r ph -> GameRules l cn r ph pl i -> [GameNode l cn r ph pl i] -> IO (GameData l cn r ph)
runNodesAgainstState gd grules nodes = do
    gen <- newCryptoRNGState
    withStdOutLogger $ \stdOutLogger -> runEff . R.runReader grules . runCryptoRNG gen . chooseRandom . runLog "main" stdOutLogger defaultLogLevel . S.execState gd $ (runMaybeT . runFromSeeds $ nodes)

instance Num a => Num (ObserveGame l cn r ph i a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = return . fromInteger
  negate = fmap negate


