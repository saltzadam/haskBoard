{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Game.GameE
    (playGameTurns)
    where

import Control.Lens  ((^.), to )
import Data.Bitraversable
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Tree (unfoldForestM)
import Effectful
import Effectful.Crypto.RNG
  ( CryptoRNG (..),
    RNG (..),
  )
import FinitaryMap (ftAt)
import Data.Finitary (Finitary)
import GHC.Generics (Generic)
import Log
import Game.Options
import Game.GameNode (GameAction (..), GameNode)
import Game.Location (LocationShape (..), decrement, increment, inventory, setCounter, transfer)
import TreeMonad
import Game.Visibility (makeVisible, makeInvisible)
import System.Random.Shuffle (shuffle)
import qualified Data.Foldable as F
import Control.Monad (replicateM, void, when, join)
import qualified Data.Sequence as Seq
import Game.GameState
import Game.Choose
import qualified Effectful.State.Static.Shared as State
import Control.Monad.Trans (lift)
import Control.Monad.Except (ExceptT(..))
import Util (ifM)


-- TODO: (fun) some kind of history besides log

updateGS :: (ObserveGame l cn r ph pl i es, Interface l cn r ph pl i :> es) => Eff es ()
updateGS = getGameState >>= update

logAction2 :: (ObserveGame l cn r ph pl i es, Log2 :> es, Show cn, Show ph, Show val, Ord r, Eq l, Show r, Show l) => GameAction l cn r ph -> val -> Eff es ()
logAction2 (IncrementCounter cn) val = logComponent (T.pack $ "Incremented " ++ show cn ++ " to " ++ show val)
logAction2 (DecrementCounter cn) val = logComponent (T.pack $ "Decremented " ++ show cn ++ " to " ++ show val)
logAction2 (SetCounter cn i) _ = logComponent (T.pack $ "Set " ++ show cn ++ " to " ++ show i)
logAction2 (RollCounter cn) val = logComponent (T.pack $ "Rolled " ++ show cn ++ " to " ++ show val)
logAction2 (Shuffle l) _ = logComponent (T.pack $ "Shuffled " ++ show l)
-- logAction2 (ChangePhase ph) _ = logComponent (T.pack $ "Changed phase to " ++ show ph)
logAction2 (MakeVisibleTo l p) _ = logComponent (T.pack $ "Made " ++ show l ++ "visible to " ++ show p)
logAction2 (MakeInvisibleTo l p) _ = logComponent (T.pack $ "Made " ++ show l ++ "invisible to " ++ show p)
logAction2 EndPhase _ = logComponent (T.pack "Ended phase")
logAction2 DoNothing _ = pure ()
logAction2 AdvanceTurn _ = logComponent "Advanced turn"
logAction2 (EndGame winners) _ = logComponent (T.pack ("Game over! Winners: " ++ show winners))
logAction2 (MkTransfer l l' r) _ = do
  invl <- show . inventory <$> useGameState (location l)
  invl' <- show . inventory <$> useGameState (location l')
  logComponent
    ( T.pack $
        "Transfered "
          ++ show r
          ++ " from "
          ++ show l
          ++ " to "
          ++ show l'
          ++ "\n Contents of "
          ++ show l
          ++ ": "
          ++ invl
          ++ "\n Contents of "
          ++ show l'
          ++ ": "
          ++ invl'
    )

act :: forall l r cn ph pl i es. (Ord l, Ord r, RNG :> es, ObserveGame l cn r ph pl i es, Eq cn, Show ph, Show cn, Show l, Show r, Log2 :> es, Eq ph, Interface l cn r ph pl i :> es) => GameAction l cn r ph -> Eff es (Maybe PhaseControl)
act DoNothing = continueGame
act a@(MkTransfer l l' r) =
  modifyingGameState (#objects . #locations) (transfer r l l')
    >> updateGS
    >> logAction2 a ' '
    >> continueGame
act a@(IncrementCounter c) =
  modifyingGameState (counter c) increment
    >> updateGS
    >> useGameState (counter c . #val)
    >>= logAction2 a
    >> continueGame
act a@(DecrementCounter c) =
  modifyingGameState (counter c) decrement
    >> updateGS
    >> useGameState (counter c . #val)
    >>= logAction2 a
    >> continueGame
act a@(SetCounter c v) =
  modifyingGameState (counter c) (`setCounter` v)
    >> updateGS
    >> useGameState (counter c . #val)
    >>= logAction2 a
    >> continueGame
act a@(RollCounter c) = do
  (bl, bu) <- useGameState (counter c . #bounds)
  newVal <- randomR (bl, bu)
  assignGameState (counterVal c) newVal
  _ <- useGameState (counterVal c) >>= logAction2 a
  updateGS
  continueGame
act a@(Shuffle l) = do
    loc <- useGameState (#objects . #locations . ftAt l)
    case loc of
      Deck cards -> do
        uniformSample <- replicateM (length cards - 1) (randomR (0, length cards - 1))
        let shuffled = Seq.fromList $ shuffle (F.toList cards) uniformSample
        assignGameState (#objects . #locations . ftAt l) (Deck shuffled)
      _ -> pure ()
    logAction2 a ' '
    updateGS
    continueGame

act a@(MakeVisibleTo p lc) =
    modifyVisibility (\vis -> makeVisible vis p lc)
    >> logAction2 a ' '
    >> updateGS
    >> continueGame
act a@(MakeInvisibleTo p lc) =
    modifyVisibility (\vis -> makeInvisible vis p lc)
    >> logAction2 a ' '
    >> updateGS
    >> continueGame
act a@EndPhase = do
    logAction2 a ' '
    return (Just PCEndPhase)
act a@AdvanceTurn = do
    logAction2 a ' '
    return (Just PCEndPhase)
act a@(EndGame _) = logAction2 a ' ' >> return (Just PCEndGame)



chooseNode :: forall l cn r ph pl i es. (Interface l cn r ph pl i :> es, GameInteract l cn r ph pl i :> es, Show pl, Show i, Show l, Show r, Show cn, Show ph, Log2 :> es, GameRun l cn r ph pl i :> es) => Eff es (Options pl i) -> Eff es [Eff es [GameNode l cn r ph pl i]]
chooseNode cs = do
       -- gs <- getGameState
       runner <- getRunner
       options <- cs
       logGame (T.pack ("Choosing from " ++ displayOptions options))
       c <- choose options
       logGame (T.pack ("Chose " ++ show c))
       return $ fmap inject $ runner c

runNode :: forall l r cn ph pl es i. (Ord l, Ord r, Ord cn, Finitary cn, Interface l cn r ph pl i :> es, ObserveGame l cn r ph pl i es, RNG :> es, Show ph, Show cn, Show l, Show r, Show pl, Show i, Log2 :> es, Eq ph, BroadcastState l cn r ph pl i :> es, GameRun l cn r ph pl i :> es) => GameNode l cn r ph pl i -> Eff es (Either PhaseControl [Eff es  [GameNode l cn r ph pl i]])
runNode aNode = maybeLeftToEmptyRight <$> bitraverse act handleChoice (aNode ^. #node)
  where

    -- TODO: layers of Monad stuff that don't need to be here?
    handleChoice :: Options pl i
                  -> Eff
                       es
                       [Eff es  [GameNode l cn r ph pl i]]
    handleChoice = chooseNode .  pure

    maybeLeftToEmptyRight :: Monoid b => Either (Maybe a) b -> Either a b
    maybeLeftToEmptyRight (Left Nothing) = Right mempty
    maybeLeftToEmptyRight (Left (Just i)) = Left i
    maybeLeftToEmptyRight (Right x) = Right x

-- pureState :: (a -> b) -> (a -> (b, a))
-- pureState f x = (f x, x)

-- embedNodeL :: (GameInteract l cn r ph pl i :> es) => [GameState l cn r ph pl i -> GameNode l cn r ph pl i] -> [Eff es (GameNode l cn r ph pl i)]
-- embedNodeL = fmap (<$> getGameState)


-- runNodeStatefully :: (GameState l cn r ph pl i -> GameNode l cn r ph pl i)
--     -> Eff es (Either
--                     PhaseControl [Eff es [GameNode l cn r ph pl i]])
-- runNodeStatefully fgn = getGameState >>= (runNode . fgn)

-- runEffFNodeStatefully :: Eff es (GameState l cn r ph pl i -> GameNode l cn r ph pl i) -> Eff es (Either PhaseControl [ Eff es [GameNode l cn r ph pl i ]])
-- runEffFNodeStatefully effn = effn >>= runNodeStatefully


runPhaseNodes :: forall l r cn ph pl es i. (Ord l, Ord r, Ord cn, Finitary cn, ObserveGame l cn r ph pl i es, Interface l cn r ph pl i :> es, RNG :> es, Show ph, Show cn, Show l, Show r, Show pl, Show i, Log2 :> es, Eq ph, BroadcastState l cn r ph pl i :> es, GameRun l cn r ph pl i :> es) => [Eff es [GameNode l cn r ph pl i]] -> Eff es PhaseControl
runPhaseNodes [] = return PCEndPhase
runPhaseNodes (node:nodes) = do
  -- theTree <- unfoldForestM unfoldSingleNode $ nodes
  result <- unfoldNodes node
  downResult <- handleResult result
  case downResult of
    PCContinue -> runPhaseNodes nodes
    i -> return i
  where
    handleResult :: Either
                        PhaseControl [Eff es [GameNode l cn r ph pl i]]
                      -> Eff es PhaseControl
    handleResult result = case result of
        Left control -> return control
        Right nextLevelnodes ->
            if null nextLevelnodes
            then return PCContinue
            else runPhaseNodes nextLevelnodes

    unfoldNodes :: Eff es [GameNode l cn r ph pl i] -> (Eff es) (Either PhaseControl [Eff es [GameNode l cn r ph pl i]])
    unfoldNodes effNodes = do
      nodes <- effNodes
      res <- traverse  runNode  nodes
      return $ fmap join . sequence $ res
      -- node <- effNode
      -- let result = runNode node
      -- (node ,) <$> result


-- TODO: improve
continueGame :: Eff es (Maybe PhaseControl)
continueGame = return Nothing


-- all control stuff

-- TODO: probably redundant w/ GameControl
data PhaseControl = PCContinue | PCEndPhase | PCEndTurn | PCEndGame deriving (Eq, Ord, Show, Generic)
-- data GameControl ph = CutoffPhase | CutoffTurn | End deriving (Eq, Ord, Show, Generic)
-- TODO: also redundant
data TurnControl = TEndTurn | TEndGame deriving (Eq, Ord, Show, Generic)

runFromPhases :: (GameInteract l cn r ph pl i :> es, Ord l, Finitary cn, Show ph, Interface l cn r ph pl i :> es, RNG :> es, Log2 :> es, Ord r, Eq ph, Show cn, Ord cn, Show l, Show r, Show pl, Show i, BroadcastState l cn r ph pl i :> es, GameRun l cn r ph pl i :> es) => [ph] -> Eff es TurnControl
runFromPhases (phase:theRest) = do
    assignGameState #currentPhase phase
    phases <- getPhases
    let newNodes = getPhaseNodes (phases phase)
    result <- runPhaseNodes newNodes
    case result of
      PCEndPhase -> runFromPhases theRest
      PCEndTurn -> return TEndTurn
      PCEndGame -> return TEndGame
      PCContinue -> runFromPhases theRest
runFromPhases [] = return TEndTurn


runTurns :: (GameInteract l cn r ph pl i :> es, Finitary cn, Interface l cn r ph pl i :> es, RNG :> es, Log2 :> es, Ord l, Ord r, Eq ph, Ord cn, Show ph, Show cn, Show l, Show r, Show pl, Show i, BroadcastState l cn r ph pl i :> es, GameRun l cn r ph pl i :> es) => Turn ph -> Eff es TurnControl
runTurns turn  = do
    let phases = NE.toList (turn ^. #turnPhases)
    runFromPhases phases


playGameTurns :: forall l cn r ph pl es i. (Ord l, Ord r, Ord cn, Finitary cn, RNG :> es, Interface l cn r ph pl i :> es, ObserveGame l cn r ph pl i es, Show ph, Show cn, Show l, Show r, Show pl, Show i, Log2 :> es, Eq ph, BroadcastState l cn r ph pl i :> es, GameRun l cn r ph pl i :> es) => Eff es (GameState l cn r ph pl i)
playGameTurns = do
    getSetupNodes <- getSetup
    gs <- getGameState
    -- TODO: no just have Game hold the setup
    let setupNodes = getSetupNodes (gs ^. #players . to length)
    void $ runPhaseNodes [pure setupNodes]
    void playGameTurns'
    getGameState
        where
            playGameTurns' = do
                gs <- getGameState
                let currentTurn = gs ^. #currentTurn
                result <- runTurns currentTurn
                case result of
                  TEndGame -> return TEndGame
                  TEndTurn -> act AdvanceTurn >> playGameTurns'

