module Game.GameNode 
    where
import GHC.Generics (Generic)
import Game.Options
import Game.Player
import Game.Visibility (VisData)
import Control.Lens ((^.))
 
-- These are the fundamental actions in a game. All the "verbs" of a game (besides the observations, e.g. "check" and "count") can be phrased in terms of these.
data GameAction l cn r ph
  = DoNothing
  | MkTransfer l l r
  | MkSwap l l r r
  | IncrementCounter cn
  | DecrementCounter cn
  | SetCounter cn Int
  | RollCounter cn
  | Shuffle l
  -- | ChangePhase ph
  | EndPhase
  | AdvanceTurn
  | MakeVisibleTo Player (VisData l cn ph)
  | MakeInvisibleTo  Player (VisData l cn ph)
  | EndGame [Player]
  deriving (Eq, Ord, Show, Generic)

-- TODO: GameAction has no owner, so should just be newtype on
-- Either (GameAction) (Options pl i, Player)
data GameNode l cn r ph pl i = GameNode
  { node :: Either  (GameAction l cn r ph) (Options pl i),
    owner :: Maybe Player
  }
  deriving (Generic, Show)


mkActionNode :: GameAction l cn r ph -> GameNode l cn r ph pl i
mkActionNode action = GameNode (Left action) Nothing

mkOptionsNode ::  Options pl i -> GameNode l cn r ph pl i
mkOptionsNode choice = GameNode (Right choice) (Just (choice ^. #owner))


