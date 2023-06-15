module Objects
    where
import GHC.Generics (Generic)
import Game.Player
import Game.Location
import Data.Set (Set)
import qualified Data.Sequence as Seq
import qualified Data.Map as M
import FinitaryMap (FTMap(..))
import Data.Void (absurd, Void)
import Game.GameState
import Game.GameNode (GameNode)
import Game.Options (Options(..), Legality(..))
import Game.Monad (GameEff)
import Game.View (GameStateView)
import Data.Map (Map)
import Data.Maybe (fromJust)
import qualified Data.List.NonEmpty as NE
import Util ( buildSafeNonempty, cartesianProduct )
data Character = Guard | Priest | Baron |
    Handmaid | Prince | King | Countess | Princess
            deriving (Eq, Ord, Show, Generic, Enum, Bounded)

charStrength :: Character -> Int
charStrength char = fromEnum char + 1



startingCards :: [LLResource]
startingCards = Card <$> [Princess, Countess, King, Prince, Prince,
    Handmaid, Handmaid, Baron, Baron, Priest, Priest,
    Guard, Guard, Guard, Guard, Guard]

data LLResource = Token | HandmaidMarker | Card Character deriving (Eq, Ord, Show, Generic)

extractChar :: LLResource -> Maybe Character
extractChar (Card char) = Just char
extractChar _ = Nothing

data LLLocation = PlayedCard
    | TheDeck
    | Hand Player
    | HandmaidInd Player
    | Tokens Player
    | BoxTop
    deriving (Eq, Ord, Show, Generic)


type LLCounters = Void

type LLGameObjects = GameObjects LLLocation LLCounters LLResource

initLocations' :: Set Player -> LLLocation -> LocationShape LLResource
initLocations' _ PlayedCard = Slot Nothing
initLocations' _ TheDeck = Deck (Seq.fromList startingCards)
initLocations' _ (Tokens _) = Pile M.empty
initLocations' _ (Hand _) = Pile M.empty
initLocations' _ (Hand _) = Pile M.empty
initLocations' _ (HandmaidInd _) = Slot Nothing
initLocations' _  BoxTop= Pile $ M.fromList [(Token, 16), (HandmaidMarker, 5)]

initLocations :: Set Player -> FTMap LLLocation (LocationShape LLResource)
initLocations = FTMap . initLocations'

initGameObjects :: Set Player -> LLGameObjects
initGameObjects ps =
    GameObjects
        { locations = initLocations ps,
          counters = FTMap absurd
        }

data LLIssue = ProtectedByHandmaid | MustDiscardCountess | OtherValidTarget deriving (Eq, Ord, Show, Generic)
data LLPlayName =  PlayPrincess
                    | PlayCountess
                    | PlayKing (Maybe Player)
                    | PlayPrince Player
                    | PlayHandmaid
                    | PlayBaron (Maybe Player)
                    | PlayPriest (Maybe Player)
                    | PlayGuard (Maybe (Player, Character))
                    deriving (Eq, Ord, Show, Generic)

target :: LLPlayName -> Maybe Player
target (PlayKing p) = p
target (PlayPrince p) = Just p
target (PlayBaron p) = p
target (PlayPriest p) = p
target (PlayGuard (Just (p,_))) = Just p
target _ = Nothing

buildPlay' :: Player -- ^ active player
           -> [Player] -- ^ other players
           -> [Player] -- ^ valid targets
           -> Character -- ^ char in hand
           -> Options LLPlayName LLIssue
buildPlay' p _ _ Princess = Options (NE.singleton PlayPrincess) M.empty p
buildPlay' p _ _ Countess = Options (NE.singleton PlayCountess) M.empty p
buildPlay' p _ _ Handmaid = Options (NE.singleton PlayHandmaid) M.empty p
-- TODO: fromJust
buildPlay' p ps tars Prince = Options
                              (PlayPrince <$>  (p NE.:| tars))
                              (M.fromList [(PlayPrince p', Illegal $ NE.singleton ProtectedByHandmaid) | p' <- ps, p' `notElem` tars])
                              p
buildPlay' p ps tars King = whenTargets PlayKing p ps tars
buildPlay' p ps tars Baron = whenTargets PlayBaron p ps tars
buildPlay' p ps tars Priest = whenTargets PlayPriest p ps tars
buildPlay' p ps tars Guard = Options
                             (buildSafeNonempty (PlayGuard . Just <$> cartesianProduct tars [Guard .. King]) (PlayGuard Nothing))
                             (M.fromList [(PlayGuard (Just (p', char)), Illegal $ NE.singleton ProtectedByHandmaid) | p' <- ps, p' `notElem` tars, char <- [Guard .. King]])
                             p

buildPlay :: Player -> [Player] -> [Player] -> Character -> Character -> Options LLPlayName LLIssue
buildPlay p ps tars Countess King = let
                                Options legal illegal _ = buildPlay' p ps tars Countess
                            in
                                Options legal (illegal <> M.fromList [(PlayKing (Just p'), Illegal (NE.singleton MustDiscardCountess)) | p' <- tars]) p
buildPlay p ps tars Countess Prince = let
                                Options legal illegal _ = buildPlay' p ps tars Countess
                            in
                                Options legal (illegal <> M.fromList [(PlayPrince p', Illegal (NE.singleton MustDiscardCountess)) | p' <- tars]) p
buildPlay p ps tars char0 char1 = buildPlay' p ps tars char0 <> buildPlay' p ps tars char1


whenTargets :: (Ord pl, Eq a) => (Maybe a -> pl) -> Player -> [a] -> [a] -> Options pl LLIssue
whenTargets f p ps tars = Options (buildSafeNonempty (f . Just <$> tars) (f Nothing))
                        (M.fromList [(f (Just p'), Illegal $ NE.singleton ProtectedByHandmaid) | p' <- ps, p' `notElem` tars])
                        p

data LLPhaseName = Setup | LLTurn Player deriving (Eq, Ord, Show, Generic)
type LLTurn = Turn LLPhaseName
type LLPhase = Phase LLPhaseName LLLocation LLCounters LLResource LLPlayName LLIssue
type LLGameState = GameState LLLocation LLCounters LLResource LLPhaseName LLPlayName LLIssue

type LLOptions = Options LLPlayName LLIssue

type LLGameRules = GameRules LLLocation LLCounters LLResource LLPhaseName LLPlayName LLIssue
type LLGameNode = GameNode LLLocation LLCounters LLResource LLPhaseName LLPlayName LLIssue

type LLM a = GameEff LLLocation LLCounters LLResource LLPhaseName LLPlayName LLIssue a

type LLView = GameStateView LLLocation LLCounters LLResource LLPhaseName

-- TODO: lens
currentPlayer :: LLPhaseName -> Maybe Player
currentPlayer (LLTurn p) = Just p
currentPlayer _ = Nothing

