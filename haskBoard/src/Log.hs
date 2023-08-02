{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Log where

import Control.Monad (void)
import Data.Text
import qualified Data.Text.IO as TIO
import Effectful
import Effectful.Dispatch.Dynamic (interpret, send)
import GHC.Generics (Generic)
import GHC.IO.Handle (Handle)

data LogLevel = DebugLevel | ComponentLevel | GameLevel deriving (Eq, Ord, Generic)

data LogPayload level = LogPayload
  { logLevel :: level,
    logMsg :: Text
  }
  deriving (Eq, Show, Generic, Functor)

data Log2 :: Effect where
  LogThis :: LogLevel -> Text -> Log2 m (LogPayload LogLevel)

type instance DispatchOf Log2 = 'Dynamic

logThis :: (Log2 :> es) => LogLevel -> Text -> Eff es (LogPayload LogLevel)
logThis level text = send (LogThis level text)

logDebug :: (Log2 :> es) => Text -> Eff es ()
logDebug text = void $ logThis DebugLevel text

logComponent :: (Log2 :> es) => Text -> Eff es ()
logComponent text = void (logThis ComponentLevel text)

logGame :: (Log2 :> es) => Text -> Eff es ()
logGame text = void (logThis GameLevel text)

logStdOut ::
  (IOE :> es) =>
  LogLevel ->
  Eff (Log2 : es) a ->
  Eff es a
logStdOut minLevel = interpret $ \_ -> \case
  LogThis level loggable ->
    if level >= minLevel
      then liftIO (TIO.putStrLn loggable) >> return (LogPayload level loggable)
      else return (LogPayload level loggable)

logToFile ::
  (IOE :> es) =>
  LogLevel ->
  Handle ->
  Eff (Log2 : es) a ->
  Eff es a
logToFile minLevel handle = interpret $ \_ -> \case
  LogThis level loggable ->
    if level >= minLevel
      then liftIO (TIO.hPutStrLn handle loggable) >> return (LogPayload level loggable)
      else return (LogPayload level loggable)
