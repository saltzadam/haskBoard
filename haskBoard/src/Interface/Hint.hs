module Interface.Hint (HintM, applyHints) where

import Control.Applicative (asum)
import Data.Set.NonEmpty (NESet)
import Game.Options (Options (..))
import Game.Rules (Query, runQuery)
import Game.View (GameStateView, inject)

-- | A hint reads the game state and may suggest one of the legal plays.
type HintM l cn r ph pl = NESet pl -> Query l cn r ph pl (Maybe pl)

-- | Evaluate the hints in order against a player's view and return the first suggestion.
-- Hidden locations read as 'Dummy' and hidden counters read as 0 (see 'inject').
applyHints :: GameStateView l cn r ph -> [HintM l cn r ph pl] -> Options pl -> Maybe pl
applyHints gsv hints (Options legal _) =
  let gs = inject gsv
   in asum [runQuery (h legal) gs | h <- hints]
