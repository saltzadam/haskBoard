{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Game.Display
    where
import Data.Finitary
import Location
import Game (Choice, Game (..), GameNode (..))
import Control.Monad.Trans.State (evalState)
import Control.Lens ((^.), view)
import Data.List (intercalate)
import FinitaryMap (ftAt)
import qualified Data.Foldable as F

displayLocationShape :: (Show r, Ord r) => LocationShape r -> String
displayLocationShape (Deck deck) = show (F.toList deck)
displayLocationShape p@(Pile _) = show (inventory p)
displayLocationShape (Slot mayber) = show mayber
displayLocationShape x = show x

-- displayGameState :: Game l cn r ph pl t pls -> String
-- displayGameState = 
displayLocations :: (Finitary l, Show r, Show l, Ord r) => Locations l r -> String
displayLocations locs = concatMap (\x -> show (x, displayLocationShape . view (ftAt x) $ locs)) inhabitants

displayObjects :: (Show cn, Finitary cn, Show r, Finitary l, Show l, Ord r) => GameObjects l cn r -> String
displayObjects objects = let
    diceShow = show (objects ^. #counters)
    trackShow = displayLocations (objects ^. #locations)
      in intercalate "\n" [diceShow, trackShow]

displayChoice :: Show pl => Game l cn r ph pl t pls -> Choice l cn r ph pl t pls -> String
displayChoice g ch = show $ evalState ch g

displayNode :: (Show pls, Show pl, Show l, Show r, Show cn, Show ph) => Game l cn r ph pl t pls -> GameNode l cn r ph pl t pls -> String
displayNode g (GameNode n o) = let
    ownerString = maybe "" (("Owner: " ++) . show) o
    in
    ownerString ++ either (displayChoice g) show n
-- case gn ^. #owner of
--                     Nothing -> either (displayChoice g) show gn
--                     Just o -> "Owner: " ++ show o ++ either (displayChoice g) show

displayStack :: (Show pls, Show pl, Show l, Show r, Show cn, Show ph) => Game l cn r ph pl t pls -> [GameNode l cn r ph pl t pls] -> String
displayStack g gns = intercalate "\n" (fmap (displayNode g) gns)

displayGame :: (Show pls, Finitary l, Finitary cn, Show cn, Show r, Show ph, Show t, Show l, Show pl, Ord r) => Game l cn r ph pl t pls -> String
displayGame game = intercalate "\n" [show (game ^. #players),
                                     displayObjects (game ^. #objects),
                                     show (game ^. #currentPhase),
                                     displayStack game (game ^. #currentStack),
                                     show (game ^. #turnNumber)]



