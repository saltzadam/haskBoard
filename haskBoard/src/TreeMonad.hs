{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use =<<" #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module TreeMonad where

import Effectful (Eff)
import GHC.Generics (Generic)
import GHC.Base (Applicative(..), join)


data GameControl ph = ChangePhaseTo ph | End deriving (Eq, Ord, Show, Generic)

-- TODO: improve
continueGame :: Eff es (Maybe (GameControl ph) )
continueGame = return Nothing


newtype TreeMonad l cn r ph pl i es a = TreeMonad {unTreeMonad :: Eff es (Either (GameControl ph) [a])}
    deriving (Functor)

-- TODO: surely some nice way to do this
instance Applicative (TreeMonad l cn r ph pl i es) where
    pure x = TreeMonad (pure . pure . pure $ x)
    treefs <*> treexs = TreeMonad $ do
        efffs <- unTreeMonad treefs
        effxs <- unTreeMonad treexs
        return $ liftA2 (<*>) efffs effxs

instance Monad (TreeMonad l cn r ph pl i es) where
  -- (>>=) :: TreeMonad l cn r ph pl i es a
  --   -> (a -> TreeMonad l cn r ph pl i es b)
  --   -> TreeMonad l cn r ph pl i es b
  (TreeMonad effxs) >>= treefs = let
    efffs = unTreeMonad . treefs
    in TreeMonad $ do
        xs <- effxs
        let mapped = fmap (fmap efffs) xs
        let gatherEithers = (fmap (fmap sequence . sequence) . sequence) mapped
        let joinEithers = fmap (fmap join) gatherEithers
        let gatherLists =  (fmap (fmap sequence . sequence) . sequence) joinEithers
        let joinLists = fmap (fmap join) gatherLists
        joinLists



