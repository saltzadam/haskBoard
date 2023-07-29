{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
module Game.GameState where
import Game.Player
import qualified Data.List.NonEmpty as NE
import GHC.Generics (Generic)
import Data.Set (Set)
import Game.Location
import Game.GameNode
import GHC.Base (NonEmpty)
import Game.Visibility ( VisibilityMap (..), VisibilityMap)
import Control.Lens
    ( makeFields,
      Lens',
      ASetter,
      Getting,
      over,
      view
   )
import Effectful (Eff, (:>), inject)
import qualified Effectful.State.Static.Shared as State
import qualified Effectful.Reader.Static as Reader
import qualified Data.Set as S
import Data.List (sortOn)
import Util (graph)
import FinitaryMap (ftAt)

data Turn phaseName = Turn {owner :: Player,
    turnPhases :: NE.NonEmpty phaseName} deriving (Eq, Ord, Show, Generic)

data Phase phaseName l cn r playName i = Phase
  { name :: phaseName,
    seedNodes :: [Eff '[GameInteract l cn r phaseName playName i] [GameNode l cn r phaseName playName i]]
  }
  deriving (Generic)

getPhaseNodes :: (GameInteract l cn r phaseName playName i :> es) => Phase phaseName l cn r playName i -> [Eff es [GameNode l cn r phaseName playName i]]
getPhaseNodes (Phase _ seedNodes') = fmap inject seedNodes'

type PlayRunner l cn r ph pl i = pl -> [Eff '[GameInteract l cn r ph pl i] [GameNode l cn r ph pl i]]

data GameState l cn r ph pl i = GameState
  { players :: Set Player,
    objects :: GameObjects l cn r,
    currentPhase :: ph,
    -- owner :: l -> Maybe Player,
    currentTurn :: Turn ph,
    nextTurn :: GameState l cn r ph pl i -> Turn ph,
    visibility :: VisibilityMap l cn ph
  }
  deriving (Generic)



-- data GameStateShow l cn r ph = GameStateShow
--     { players :: Set Player,
--       objects :: GameObjects l cn r,
--       currentPhase :: ph
--     } deriving (Generic, Show)

-- projectShow :: GameState l cn r ph pl i -> GameStateShow l cn r ph
-- projectShow (GameState pl obj curr _ _ _ _) = GameStateShow pl obj curr

-- instance (Finitary l, Finitary cn, Show l, Show r, Show cn, Show ph) => Show (GameState l cn r ph pl i) where
--     show = show . projectShow

data GameRules l cn r ph pl i = GameRules
  { playRunner :: PlayRunner l cn r ph pl i,
    phases :: ph -> Phase ph l cn r pl i,
    score :: GameState l cn r ph pl i -> Player -> Int,
    setupPhase :: Maybe ph
  }
  deriving (Generic)


counter :: Eq cn => cn -> Lens' (GameState l cn r ph pl i) Counter
counter c = #objects . #counters . ftAt c

counterVal :: Eq cn => cn -> Lens' (GameState l cn r ph pl i) Int
counterVal c = counter c . #val

location :: Eq l => l -> Lens' (GameState l cn r ph pl i) (LocationShape r)
location l = #objects . #locations . ftAt l

type GameInteract l cn r ph pl i = State.State (GameState l cn r ph pl i)
-- TODO: package this and eliminate Game
type GameRun l cn r ph pl i = Reader.Reader (GameRules l cn r ph pl i)

makeFields ''GameState
makeFields ''GameRules
makeFields ''Phase


getsGameState :: (GameInteract l cn r ph pl i :> es) =>  (GameState l cn r ph pl i -> b) -> Eff es b
getsGameState = State.gets

getGameState :: (GameInteract l cn r ph pl i :> es) => Eff es (GameState l cn r ph pl i)
getGameState = getsGameState id

useGameState :: (GameInteract l cn r ph pl i :> es) => Getting b (GameState l cn r ph pl i) b -> Eff es b
useGameState o = getsGameState (view o)

getRunner :: (GameRun l cn r ph pl i :> es) => Eff es (PlayRunner l cn r ph pl i)
getRunner = Reader.asks (view #playRunner)

getPhases :: (GameRun l cn r ph pl i :> es) => Eff es (ph -> Phase ph l cn r pl i)
getPhases = Reader.asks (view #phases)

getScore :: (GameRun l cn r ph pl i :> es) => Eff es (GameState l cn r ph pl i -> Player -> Int )
getScore = Reader.asks (view #score)

getSetupPhase :: (GameRun l cn r ph pl i :> es) => Eff es (Maybe ph)
getSetupPhase = Reader.asks (view #setupPhase)

modifyingGame :: (GameInteract l cn r ph pl i :> es) => ASetter (GameState l cn r ph pl i) (GameState l cn r ph pl i) a b -> (a -> b) -> Eff es ()
modifyingGame o = State.modify . over o

modifyingGameState :: (GameInteract l cn r ph pl i :> es) => ASetter  (GameState l cn r ph pl i) (GameState l cn r ph pl i) a b -> (a -> b) -> Eff es ()
modifyingGameState o = State.modify . over o

assignGameState :: (GameInteract l cn r ph pl i :> es) => ASetter  (GameState l cn r ph pl i) (GameState l cn r ph pl i) a b -> b -> Eff es ()
assignGameState l b = modifyingGameState l (const b)

getVisibility :: (GameInteract l cn r ph pl i :> es) => Eff es (VisibilityMap l cn ph)
getVisibility = useGameState #visibility

modifyVisibility :: (GameInteract l cn r ph pl i :> es) => (VisibilityMap l cn ph -> VisibilityMap l cn ph) -> Eff es ()
modifyVisibility = modifyingGame #visibility

winnerBy :: (Ord a, GameInteract l cn r ph pl i :> es, GameRun l cn r ph pl i :> es) => (Int -> a) -> Eff es [Player]
winnerBy f = do
    players <- S.toList <$> useGameState #players
    score <- getScore
    gs <- getGameState
    let scoredPlayers = sortOn snd . fmap (graph (f . score gs)) $ players
    let maxScore = snd . head $ scoredPlayers
    return $ fmap fst . takeWhile (\(_, s) -> s == maxScore) $ scoredPlayers

winner :: (GameInteract l cn r ph pl i :> es, GameRun l cn r ph pl i :> es) => Eff es [Player]
winner = winnerBy id

