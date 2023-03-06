{-# LANGUAGE DeriveGeneric #-}
module TreeControl
    where
import GHC.Generics (Generic)
import Control.Monad (MonadPlus (..))
import Data.Tree (Tree (..))
import Control.Applicative ((<|>))

data TreeControl b = TContinue | TStop | TRestart [b] deriving (Eq, Ord, Show, Generic)

unfoldTreeControl :: (Monad m, MonadPlus m) => (b -> m (a, [b], TreeControl b)) -> b -> m (Tree a)
unfoldTreeControl f b = do
    (a, bs, c) <-  f b
    case  c of
        TContinue -> do
            ts <- unfoldForestControl f bs
            return (Node a ts)
        TStop -> mzero -- will stop entire computation
        TRestart newbs ->  mzero <|>  do
            ts <- unfoldForestControl f newbs
            return (Node a ts)


unfoldForestControl :: (Monad m, MonadPlus m) => (b -> m (a, [b], TreeControl b)) -> [b] -> m [Tree a]
unfoldForestControl f =  mapM (unfoldTreeControl f)


