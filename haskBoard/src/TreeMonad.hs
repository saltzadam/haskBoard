module TreeMonad
    (TreeMonad (..),
     runTree)
    where

import Effectful (Eff)
import GHC.Base (Applicative(..), join)
import Control.Monad.Except (ExceptT(..), lift, runExceptT)

newtype TreeMonad l cn r ph pl i es control a = TreeMonad {unTreeMonad :: ExceptT control (Eff es) [a]}
    deriving (Functor)

runTree :: TreeMonad l cn r ph pl i es control a -> Eff es (Either control [a])
runTree (TreeMonad u) = runExceptT u

-- TODO: surely some nice way to do this
instance Applicative (TreeMonad l cn r ph pl i es control) where
    pure x = TreeMonad (pure [x])
    (TreeMonad treefs) <*> (TreeMonad treexs) = TreeMonad $ do
        efffs <- treefs
        effxs <- treexs
        return $ efffs <*> effxs

instance Monad (TreeMonad l cn r ph pl i es control) where
  -- (>>=) :: TreeMonad l cn r ph pl i es a
  --   -> (a -> TreeMonad l cn r ph pl i es b)
  --   -> TreeMonad l cn r ph pl i es b
  (TreeMonad effxs) >>= treefs = let
    efffs = runTree . treefs
    in TreeMonad $ do
        xs <- effxs
        ExceptT $ fmap (fmap (join . sequence) . sequence) (traverse efffs xs)



