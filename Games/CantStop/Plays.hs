module Plays where
import Objects
import Game.GameNode
import Game.Player (Player)
import Data.List (nub)
import qualified Data.List.NonEmpty as NE
import Control.Lens
import Game.Options
import Game.Helpers (doesNotHave, doesNotHave')

rollDice = action . RollCounter <$> [DieOne .. DieFour]

legalMoves :: Player -> (Int, Int, Int, Int) -> CantStopLocation -> CantStopOptions
legalMoves p diceVals locs = let
        trackPairs = NE.fromList . fmap (over both diceToTrack) . mkPairs $ diceVals
        noMarkersLeft = BoxTop `doesNotHave'` TemporaryMarker 
                in undefined
    where 
    mkPairs :: (Eq a, Num a) => (a, a, a, a) -> [(a, a)]
    mkPairs (x, y, w, z) = nub [(x + y, w + z), (x+ w, y+ z), (x+ z, y+ w)]
