module Buster.Machine.Logging (plugin) where

import Buster.IRC
import Buster.Message
import Buster.Misc
import Buster.Plugin
import Control.Monad.State
import qualified Data.Map as Map
import Data.Time
import System.Directory
import System.Locale
import System.IO

io = liftIO :: IO a -> Net a

plugin = makePlugin $ do
    let dir = "logs"
    doesDirectoryExist dir >>= (`unless` createDirectory dir)
    return $ processorPlugin $ logStub dir
  where
    logStub dir msg = gets channels >>= io . logger dir msg
 
logger dir msg chs = do
    now <- getZonedTime
    mapM_ (go now) (Map.assocs chs)
  where
    go t (ch, cs) = when (filterByChan (`Map.member` chanNames cs) msg ch) $
        let filename = dir ++ "/" ++ pretty ch ""
                       ++ "." ++ dateStr t ++ ".log"
        in appendFile filename $ (timeStr t ++) . pretty msg $ "\n"

format' = formatTime defaultTimeLocale . iso8601DateFormat
dateStr = format' Nothing
timeStr = format' (Just "%T  ")

-- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
