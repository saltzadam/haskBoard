{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
module Game.Helpers
    where
import Effectful
import Game.Player
import Game.GameNode
import Count
import Game.Location (findResourceWithin, howMany', howManyF)
import Game.GameState 
--- Helpers ---
(<+) :: Enum a => a -> Int -> a
a <+ i
  | i > 0 = succ a <+ (i - 1)
  | i < 0 = pred a <+ (i + 1)
  | otherwise = a

findResourceWithin' ::  (Ord r, GameInteract mode1 l cn1 r ph1 pl1 i1 :> es) =>  r -> [l] -> Eff es [l]
findResourceWithin' r locationNames = do
  locs <- useGameState (#objects . #locations)
  return $ findResourceWithin r locationNames locs

mkMoveNode :: Player -> GameAction l cn r ph -> GameNode l cn r ph pl i
mkMoveNode p act = GameNode (Left act) (Just p)

howManyAt :: (Ord r, Eq l, GameInteract mode l cn r ph pl i :> es) => l -> r -> Eff es (Cnt Int)
howManyAt l r = flip howMany' r <$> useGameState (location l)

has ::  (Ord r, Eq l, GameInteract mode l cn r ph pl i :> es) =>  l -> r -> Eff es Bool
has l r = (> 0) <$> howManyAt l r

doesNotHave ::  (Ord r, Eq l, GameInteract mode l cn r ph pl i :> es) =>  l -> r -> Eff es Bool
doesNotHave l r = not <$> has l r

hasAny :: (Ord r, GameInteract mode0 l cn0 r ph0 pl0 i0 :> es, Eq l) => l -> (r -> Bool) -> Eff es Bool
hasAny l filt = (> 0) . flip howManyF filt <$> useGameState (location l)

transferAll :: (Ord r, Eq l, GameInteract mode0 l cn0 r ph0 pl0 i0 :> es) => l -> l -> r -> Eff es [GameNode l cn r ph pl i]
transferAll source target res = do
    numRes <- howManyAt source res
    case numRes of
      Cnt i -> return . fmap mkActionNode $ replicate i (MkTransfer source target res)
      _ -> return . fmap mkActionNode $ repeat (MkTransfer source target res)

