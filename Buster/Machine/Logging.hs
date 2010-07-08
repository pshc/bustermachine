module Buster.Machine.Logging (plugin) where

import Buster.IRC
import Buster.Misc
import Buster.Plugin
import Control.Monad.State
import qualified Data.Map as Map
import Data.Time
import System.Locale

io = liftIO :: IO a -> Net a

plugin = makePlugin $ return $ processorPlugin logger
 
logger msg = gets channels >>= io . mapM_ go . Map.keys
  where
    go ch = do time <- getZonedTime
               let filename = "logs/" ++ ch ++ "." ++ dateStr time ++ ".log"
               putStrLn $ pretty time . ("  " ++) . pretty msg $ ""

instance Pretty ZonedTime where
    pretty = showString . timeStr

format' = formatTime defaultTimeLocale . iso8601DateFormat
dateStr = format' Nothing
timeStr = format' (Just "%T")

-- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
