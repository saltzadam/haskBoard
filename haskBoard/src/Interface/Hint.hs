module Interface.Hint

where
import Game.View (GameStateView (..), viewGameStateAs', useGameStateView, inject, injectLocs, injectCnt)
import Game.Options (Options (..))
import Game.Rules
import Control.Applicative (asum)
import Game.GameState (GameInteract)
import Effectful ((:>), Eff, runPureEff)
import qualified Effectful.State.Static.Shared as State
import Control.Monad.Free (Free (..))
import Game.Player (Turn(..), Player)
import FinitaryMap (ftAt)
import Control.Lens.Prism (_Just)
import Control.Lens (to, (&), (.~))
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NE
import Effectful.NonDet (NonDet)
import Game.Visibility (allVisible)

-- | A hint suggests a play given the visible game state and legal options.
-- Return 'Just' to suggest a specific play, 'Nothing' to defer.
-- TODO: Surface hints in the TUI (e.g. highlight the suggested play).
type Hint l cn r ph pl = GameStateView l cn r ph -> Options pl -> Maybe pl

-- applyHints :: [Hint l cn r ph pl] -> GameStateView l cn r ph -> Options pl -> Maybe pl
-- applyHints [] _ _ = Nothing
-- applyHints (h : hs) gsv opts =
--   case h gsv opts of
--     Just play -> Just play
--     Nothing -> applyHints hs gsv opts

-- applyHints :: [Hint l cn r ph pl] -> GameStateView l cn r ph -> Options pl -> Maybe pl
-- applyHints hints gsv opts = asum (fmap (\h -> h gsv opts) hints)

-- This is not safe. Hints should not have actions and they should include visibility tests. 
type HintM l cn r ph pl = NESet pl -> GameRule l cn r ph pl ( Maybe pl)

evalHint' :: (Eq l, Eq cn, GameInteract l cn r ph pl :> es) => Player -> Free (GameRuleF l cn r ph pl) a -> Eff es a
evalHint' _ (Pure a)                        = return a
evalHint' p (Free (LookLocation l k))       = useGameStateView p (#objectsView . #locationsView . to injectLocs.  ftAt l) >>= evalHint' p . k
evalHint' p (Free (LookCounter cn k))       = useGameStateView p (#objectsView . #countersView . to injectCnt . ftAt cn) >>= evalHint' p . k
evalHint' p (Free (LookPlayers k))          = useGameStateView p #playersView >>= evalHint' p . k
evalHint' p (Free (LookCurrentPhase k))     = useGameStateView p #currentPhaseView >>= evalHint' p . k
evalHint' p (Free (LookCurrentTurnOwner k)) = useGameStateView p (#currentTurnView . to (\(Turn ph _) -> ph)) >>= evalHint' p . k
evalHint' p (Free (LookGameState k))        = useGameStateView p (to inject) >>= evalHint' p . k
evalHint' _ (Free (Act _ _))                = error "evalRule: score function must not perform actions"
evalHint' _ (Free (MakeChoice _ _))         = error "evalRule: score function must not make choices"

applyHint :: (Eq l, Eq cn, GameInteract l cn r ph pl :> es) => HintM l cn r ph pl -> Options pl -> Eff es (Maybe pl)
applyHint  theHint (Options legal p) = let GameRule free = theHint legal
  in evalHint' p free 

applyHints :: (Eq l, Eq cn, GameInteract l cn r ph pl :> es) => [HintM l cn r ph pl] -> Options pl -> Eff es (Maybe pl)
applyHints [] _= return Nothing
applyHints hints opts = fmap asum $ sequence (fmap (($ opts) . applyHint) hints)

-- | Apply hints purely against a 'GameStateView' by running 'evalHint''
--   in a throwaway 'State' seeded from 'inject gsv'.  Visibility is set to
--   'allVisible' because the view already reflects the player's perspective.
applyHintsPure :: (Eq l, Eq cn) => GameStateView l cn r ph -> [HintM l cn r ph pl] -> Options pl -> Maybe pl
applyHintsPure gsv hints (Options legal p) =
  let gs0 = inject gsv & #visibility .~ allVisible
      run h = let GameRule free = h legal
              in runPureEff . State.evalState gs0 $ evalHint' p free
  in asum [run h | h <- hints]

