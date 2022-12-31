{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLabels #-}
module Game.Trigger where
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Class (lift, MonadTrans)
import Control.Monad.Trans.Writer (WriterT)
import Data.Functor.Identity
import Control.Applicative
import Control.Monad.Reader (MonadReader)
import Control.Monad.Writer (MonadWriter, tell)
import Control.Monad.RWS (ask)
import Data.Monoid (Endo(..))

-- abstract description of trigger computation

-- given environment, send a to [a]
type AbsAct e a b = e -> a -> ([a], Endo b)

newtype TriggerEnvT e a b m c = TriggerEnvT {unwrap :: WriterT [a] (ReaderT (AbsAct e a b) m) c}
    deriving (Functor, Applicative, Monad,
              MonadReader (AbsAct e a b),
              MonadWriter [a])
    -- deriving (Semigroup, Monoid) via (WriterT [a] (ReaderT (Tri a, ActionToF a b) m))

instance (Semigroup c, Monad m) => Semigroup (TriggerEnvT e a b m c) where
    (<>) = liftA2 (<>)

instance (Monoid c, Monad m) => Monoid (TriggerEnvT e a b m c) where
    mempty = TriggerEnvT (return mempty)

instance MonadTrans (TriggerEnvT e a b) where
    lift = TriggerEnvT . lift . lift

type TriggerEnv e a b c = TriggerEnvT e a b Identity c

evalWTriggersT :: e -> a -> TriggerEnv e a b (Endo b)
evalWTriggersT e a = do
    f <- ask
    let (triggers, endo) = f e a
    tell triggers
    return endo


type SelfTriggerEnv a b = TriggerEnvT b a b Identity (Endo b)

