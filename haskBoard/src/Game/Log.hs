{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use void" #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Game.Log
    where
import Effectful
import Data.Text
import qualified Data.Text.IO as TIO
import GHC.IO.Handle (Handle)
import Effectful.Dispatch.Dynamic (interpret, send)
import GHC.Generics (Generic)


data LogLevel = DebugLevel | ComponentLevel | GameLevel deriving (Eq, Ord, Generic)

data LogPayload level = LogPayload {
    logLevel :: level,
    logMsg :: Text
} deriving (Eq, Show, Generic, Functor)


data Log2 :: Effect where
    LogThis :: LogLevel -> Text -> Log2 m (LogPayload LogLevel)
type instance DispatchOf Log2 = 'Dynamic

logThis :: (Log2 :> es) => LogLevel -> Text -> Eff es (LogPayload LogLevel)
logThis level text = send (LogThis level text)

logDebug :: (Log2 :> es) => Text -> Eff es ()
logDebug text = logThis DebugLevel text >> return ()

logComponent :: (Log2 :> es) => Text -> Eff es ()
logComponent text = logThis ComponentLevel text >> return ()

logGame :: (Log2 :> es) => Text -> Eff es ()
logGame text = logThis GameLevel text >> return ()





logStdOut :: (IOE :> es) => 
    LogLevel ->
    Eff (Log2 : es) a ->
    Eff es a
logStdOut minLevel = interpret $ \_ -> \case
    LogThis level loggable -> if level >= minLevel
                              then liftIO (TIO.putStrLn loggable) >> return (LogPayload level loggable)
                              else return (LogPayload level loggable)

logToFile :: (IOE :> es) =>
    LogLevel ->
    Handle ->
    Eff (Log2 : es) a ->
    Eff es a
logToFile minLevel handle = interpret $ \_ -> \case
    LogThis level loggable -> if level >= minLevel
                              then liftIO (TIO.hPutStrLn handle loggable) >> return (LogPayload level loggable)
                              else return (LogPayload level loggable)


