{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Log
  ( LogLevel (..),
    LogPayload (..),
    Log2,
    logDebug,
    logComponent,
    logGame,
    logStdOut,
    logToFile,
    logToFileJSON,
    nullLoggerHandleText,
    nullLoggerHandle
  ) where

import Control.Monad (void)
import Data.Text
import qualified Data.Text.IO as TIO
import Effectful
import Effectful.Dispatch.Dynamic (interpret, send)
import GHC.Generics (Generic)
import GHC.IO.Handle (Handle)
import Data.Aeson (ToJSON)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text.Lazy as TL

data LogLevel = DebugLevel | ComponentLevel | GameLevel deriving (Eq, Ord, Generic)

data LogPayload level rep = LogPayload
  { logLevel :: level,
    logMsg :: rep
  }
  deriving (Eq, Show, Generic, Functor)

data Log2 rep :: Effect where
  LogThis :: LogLevel -> rep -> Log2 rep m (LogPayload LogLevel rep)

type instance DispatchOf (Log2 rep) = 'Dynamic

logThis :: (Log2 rep :> es) => LogLevel -> rep -> Eff es (LogPayload LogLevel rep )
logThis level text = send (LogThis level text)

logDebug :: (Log2 rep :> es) => rep -> Eff es ()
logDebug text = void $ logThis DebugLevel text

logComponent :: (Log2 rep :> es) => rep -> Eff es ()
logComponent text = void (logThis ComponentLevel text)

logGame :: (Log2 rep :> es) => rep -> Eff es ()
logGame text = void (logThis GameLevel text)

logStdOut ::
  (IOE :> es) =>
  LogLevel ->
  Eff (Log2 Text : es) a ->
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
  Eff (Log2 Text : es) a ->
  Eff es a
logToFile minLevel handle = interpret $ \_ -> \case
  LogThis level loggable ->
    if level >= minLevel
      then liftIO (TIO.hPutStrLn handle loggable) >> return (LogPayload level loggable)
      else return (LogPayload level loggable)

logToFileJSON  ::
  (IOE :> es, ToJSON b) =>
  LogLevel ->
  Handle ->
  Eff (Log2 b : es) a ->
  Eff es a
logToFileJSON minLevel handle = interpret $ \_ -> \case
  LogThis level jsonable ->
    if level >= minLevel
      then liftIO (TIO.hPutStrLn handle (TL.toStrict (encodeToLazyText jsonable))) >> return (LogPayload level jsonable)
      else return (LogPayload level jsonable)

-- Not ideal but would rather stuff it here
nullLoggerHandleText :: 
  (IOE :> es) =>
  LogLevel ->
  Eff (Log2 Text : es) a ->
  Eff es a
nullLoggerHandleText _minLevel = interpret $ \_ -> \case
  LogThis level text -> return (LogPayload level text )

nullLoggerHandle :: 
  (IOE :> es) =>
  LogLevel ->
  Eff (Log2 rep : es) a ->
  Eff es a
nullLoggerHandle _minLevel = interpret $ \_ -> \case
  LogThis level jsonable -> return (LogPayload level jsonable)


