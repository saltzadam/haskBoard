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
import Game.View (GameStateViewC)
import Control.Concurrent (Chan, writeChan, readChan)
import Data.Kind (Type)

data Interface (l :: Type) (cn :: Type) (r :: Type) (ph :: Type) (pl :: Type) (i :: Type) :: Effect where
  Choose :: Options pl i -> (Interface l cn r ph pl i) m pl
  Update :: GameStateViewC l cn r ph pl i -> (Interface l cn r ph pl i) m ()

type instance DispatchOf (Interface l cn r ph pl i) = 'Dynamic

choose :: (Interface l cn r ph pl i :> es) =>  Options pl i -> Eff es pl
choose cs = send (Choose cs) 

update :: (Interface l cn r ph pl i :> es) => GameStateViewC l cn r ph pl i -> Eff es ()
update gsvc = send (Update gsvc)

chooseFirst :: Eff (Interface l cn r ph pl i : es) a -> Eff es a
chooseFirst = interpret $ \_ -> \case
  Choose cs -> return (cs ^. #legal . to NE.head)
  Update _ -> return ()

chooseRandom :: (RNG :> es) => Eff (Interface l cn r ph pl i : es) a -> Eff es a
chooseRandom = interpret $ \_ -> \case
  Choose cs' ->
    let cs = cs' ^. #legal
        choice = randomR (0, length cs - 1)
     in fmap (cs NE.!!) choice
  Update _ -> return ()

chooseBasicInput ::  (IOE :> es) => Eff (Interface l cn r ph pl i : es) a -> Eff es a
chooseBasicInput = interpret $ \_ -> \case
  Choose cs' -> do
    let cs = cs' ^. #legal . to NE.toList
    liftIO $ loopChoice cs
  Update gs -> return ()
  where
    loopChoice cs = do
      c <- liftIO getChar
      case readMaybe [c] :: Maybe Int of
        Nothing -> putStrLn "couldn't parse" >> loopChoice cs
        Just i -> case listToMaybe (drop (i - 1) cs) of
          Just pl -> return pl
          Nothing -> putStrLn "couldn't find" >> loopChoice cs

chooseChan :: IOE :> es => Chan (Either (GameStateViewC l cn r ph pl i)  (Options pl i))
            -> Chan pl
            -> Eff (Interface l cn r ph pl i : es) pl
            -> Eff es pl
chooseChan gameToClientChan clientToGameChan = interpret $ \_ -> \case
    Update gsvc -> liftIO $ writeChan gameToClientChan (Left gsvc)
    Choose options -> liftIO $ do
        writeChan gameToClientChan (Right options)
        readChan clientToGameChan
        
