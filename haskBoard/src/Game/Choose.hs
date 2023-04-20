{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Game.Choose where
import Effectful
import Game.GameState
import Game.Options (Options)
import Effectful.Crypto.RNG (RNG, randomR)
import qualified Data.List.NonEmpty as NE
import Effectful.Dispatch.Dynamic
import Control.Lens
import Text.Read (readMaybe)
import Data.Maybe (listToMaybe)

data Choosing :: Effect where
  Choose :: GameState l cn r ph pl i -> Options pl i -> Choosing m pl

type instance DispatchOf Choosing = 'Dynamic

choose :: forall l cn r ph pl mode es i. (Choosing :> es, GameInteract mode l cn r ph pl i :> es) => Options pl i -> Eff es pl
choose cs = getGameState >>= \g -> send (Choose g cs)

chooseFirst :: forall es pl. Eff (Choosing : es) pl -> Eff es pl
chooseFirst = interpret $ \_ -> \case
  Choose _ cs -> return (cs ^. #legal . to NE.head)

chooseRandom :: (RNG :> es) => Eff (Choosing : es) pl -> Eff es pl
chooseRandom = interpret $ \_ -> \case
  Choose _ cs' ->
    let cs = cs' ^. #legal
        choice = randomR (0, length cs - 1)
     in fmap (cs NE.!!) choice

chooseBasicInput :: forall pl es. (IOE :> es) => Eff (Choosing : es) pl -> Eff es pl
chooseBasicInput = interpret $ \_ -> \case
  Choose _ cs' -> do
    let cs = cs' ^. #legal . to NE.toList
    liftIO $ loopChoice cs
  where
    loopChoice cs = do
      c <- liftIO getChar
      case readMaybe [c] :: Maybe Int of
        Nothing -> putStrLn "couldn't parse" >> loopChoice cs
        Just i -> case listToMaybe (drop (i - 1) cs) of
          Just pl -> return pl
          Nothing -> putStrLn "couldn't find" >> loopChoice cs

