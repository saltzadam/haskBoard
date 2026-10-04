module Log (LogTag (..), Logger, mkLogger, nullLogger) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)

data LogTag = ActionLog | ChoiceLog | WinnersLog
  deriving (Eq, Ord, Show)

-- | Writes a log line to the destination for its tag.
type Logger = LogTag -> Text -> IO ()

-- | Send each tag to its writer and drop tags that have no writer.
mkLogger :: Map LogTag (Text -> IO ()) -> Logger
mkLogger writers tag msg = maybe (pure ()) ($ msg) (M.lookup tag writers)

nullLogger :: Logger
nullLogger _ _ = pure ()
