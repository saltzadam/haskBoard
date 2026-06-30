{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Log
  ( LogTag (..),
    Log2,
    logComponent,
    logChoice,
    logWinners,
    runLogger,
    nullLogger,
  ) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Effectful
import Effectful.Dispatch.Dynamic (interpret, send)

data LogTag = ActionLog | ChoiceLog | WinnersLog
  deriving (Eq, Ord, Show)

data Log2 :: Effect where
  LogThis :: LogTag -> Text -> Log2 m ()

type instance DispatchOf Log2 = 'Dynamic

logComponent :: (Log2 :> es) => Text -> Eff es ()
logComponent text = send (LogThis ActionLog text)

logChoice :: (Log2 :> es) => Text -> Eff es ()
logChoice text = send (LogThis ChoiceLog text)

logWinners :: (Log2 :> es) => Text -> Eff es ()
logWinners text = send (LogThis WinnersLog text)

runLogger ::
  (IOE :> es) =>
  Map LogTag (Text -> IO ()) ->
  Eff (Log2 : es) a ->
  Eff es a
runLogger writers = interpret $ \_ -> \case
  LogThis tag msg ->
    case M.lookup tag writers of
      Just write -> liftIO (write msg)
      Nothing    -> pure ()

nullLogger :: (IOE :> es) => Eff (Log2 : es) a -> Eff es a
nullLogger = runLogger M.empty
