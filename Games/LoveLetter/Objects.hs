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
import Game.Options (Options)
import Game.Monad (GameEff)
import Game.View (GameStateView)

data Character = Guard | Priest | Baron |
    Handmaid | Prince | King | Countess | Princess
            deriving (Eq, Ord, Show, Generic, Enum)

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
    | TokenPile
    | Hand Player
    | HandmaidInd Player
    | Tokens Player
    | DiscardPile
    deriving (Eq, Ord, Show, Generic)


type LLCounters = Void

type LLGameObjects = GameObjects LLLocation LLCounters LLResource

initLocations' :: Set Player -> LLLocation -> LocationShape LLResource
initLocations' _ PlayedCard = Slot Nothing
initLocations' _ TheDeck = Deck (Seq.fromList startingCards)
initLocations' _ TokenPile = Pile (M.singleton Token 16)
initLocations' _ (Tokens _) = Pile M.empty
initLocations' _ (Hand _) = Pile M.empty
initLocations' _ (HandmaidInd _) = Slot Nothing
initLocations' _ DiscardPile = Pile M.empty

initLocations :: Set Player -> FTMap LLLocation (LocationShape LLResource)
initLocations = FTMap . initLocations'

initGameObjects :: Set Player -> LLGameObjects
initGameObjects ps =
    GameObjects
        { locations = initLocations ps,
          counters = FTMap absurd
        }

data LLIssue = MustDiscardCountess deriving (Eq, Ord, Show, Generic)
data LLPlayName =  PlayPrincess
                    | PlayCountess
                    | PlayKing (Maybe Player)
                    | PlayPrince Player
                    | PlayHandmaid
                    | PlayBaron (Maybe Player)
                    | PlayPriest (Maybe Player)
                    | PlayGuard (Maybe Player)
                    deriving (Eq, Ord, Show, Generic)
                    

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

