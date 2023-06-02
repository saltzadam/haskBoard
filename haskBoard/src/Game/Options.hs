{-# LANGUAGE TemplateHaskell #-}
module Game.Options where
import GHC.Generics (Generic)
import Data.List.NonEmpty ( NonEmpty )
import Control.Lens (makeFields)
import Data.Map (Map)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Foldable as F
import Util (graphM, buildSafeNonempty)
import Game.Player (Player)

-- A move may have lots of illegality.
-- Concatenate where possible.

data Legality illegal = Legal | Illegal [illegal] deriving (Eq, Ord, Show, Generic, Functor)

instance Semigroup (Legality i) where
    Legal <> x = x
    x <> Legal = x
    Illegal x <> Illegal y = Illegal (x <> y)

firstLegal :: Eq i => (pl -> Legality i) -> [pl] -> Maybe pl
firstLegal check (play:plays) = if check play == Legal
                                then Just play
                                else firstLegal check plays 
firstLegal _ [] = Nothing

firstLegalM :: (Eq illegal, Monad m) => (b -> m (Legality illegal)) -> [b] -> m (Maybe b)
firstLegalM check (play:plays) = do
    checkM <- check play
    if checkM == Legal
    then return $ Just play
    else firstLegalM check plays
firstLegalM _ [] = return Nothing

instance Monoid (Legality i) where
    mempty = Legal

data Options pl i = Options {legal :: NonEmpty pl,
                             illegal :: Map pl (Legality i),
                             owner :: Player
                            } deriving (Eq, Ord, Show, Generic)

makeFields ''Options

instance (Ord pl) => Semigroup (Options pl i) where
    (Options legal illegal owner) <> (Options legal' illegal' _) = Options (legal <> legal') (illegal <> illegal') owner

raiseIssueIf :: issue -> Bool -> Legality issue
raiseIssueIf = flip mustNotElse

mustElse :: Bool -> issue -> Legality issue
mustElse True _ = Legal
mustElse False i = Illegal [i]

mustNotElse :: Bool -> issue -> Legality issue
mustNotElse True i = Illegal [i]
mustNotElse False _ = Legal

buildOptions :: (Monad m, Traversable t, Eq issue, Ord play) => Player -> (play -> m (Legality issue)) -> play -> m (t play) -> m (Options play issue)
buildOptions p checkPlay defaultPlay plays = do
    thePlays <- plays
    legalities <- traverse (graphM checkPlay) thePlays
    let (legalMoves, illegalMoves) = M.partition (== Legal) . M.fromList . F.toList $ legalities
    let legalMovesWithDefault = buildSafeNonempty (M.keys legalMoves) defaultPlay    
    return (Options legalMovesWithDefault illegalMoves p)

displayOptions :: (Show pl, Show i) => Options pl i -> String
displayOptions (Options legalO illegalO p) =
    show (NE.toList legalO) ++ "\n"
    ++ "Cannot choose: " ++ showMapAsList illegalO
        where
            showMapAsList :: (Show pl, Show i) => Map pl (Legality i) -> String
            showMapAsList = show . M.toList
