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
import Game.Monad (GameM, viewGameState)
--- Helpers ---
(<+) :: Enum a => a -> Int -> a
a <+ i
  | i > 0 = succ a <+ (i - 1)
  | i < 0 = pred a <+ (i + 1)
  | otherwise = a

findResourceWithin' ::  (Ord r) =>  r -> [l] -> GameM l cn r ph pl i [l]
findResourceWithin' r locationNames = do
  locs <- viewGameState (#objects . #locations)
  return $ findResourceWithin r locationNames locs

mkMoveNode :: Player -> GameAction l cn r ph -> GameNode l cn r ph pl i
mkMoveNode p act = GameNode (Left act) (Just p)

howManyAt :: (Ord r, Eq l) => l -> r -> GameM l cn r ph pl i (Cnt Int)
howManyAt l r = flip howMany' r <$> viewGameState (location l)

has ::  (Ord r, Eq l) =>  l -> r -> GameM l cn r ph pl i Bool
has l r = (> 0) <$> howManyAt l r

doesNotHave ::  (Ord r, Eq l) =>  l -> r -> GameM l cn r ph pl i Bool
doesNotHave l r = not <$> has l r

hasAny :: (Ord r, Eq l) => l -> (r -> Bool) -> GameM l cn r ph pl i Bool
hasAny l filt = (> 0) . flip howManyF filt <$> viewGameState (location l)

transferAll :: (Ord r, Eq l) => l -> l -> r -> GameM l cn r ph pl i [GameNode l cn r ph pl i]
transferAll source target res = do
    numRes <- howManyAt source res
    case numRes of
      Cnt i -> return . fmap mkActionNode $ replicate i (MkTransfer source target res)
      _ -> return . fmap mkActionNode $ repeat (MkTransfer source target res)

