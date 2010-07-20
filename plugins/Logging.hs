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

main = do let dir = "logs"
          doesDirectoryExist dir >>= (`unless` createDirectory dir)
          pluginMain $ processorPlugin (logStub dir)
  where
    logStub dir msg = queryChans >>= io . logger dir msg
 
logger dir msg chs = do
    now <- getZonedTime
    mapM_ (go now) (Map.assocs chs)
  where
    go t (ch, cs) = when inChannel $
        let filename = dir ++ "/" ++ pretty ch ""
                       ++ "." ++ dateStr t ++ ".log"
        in appendFile filename $ (timeStr t ++) . pretty msg $ "\n"
      where
        inChannel = filterByChan (`Map.member` chanUsers cs) False msg ch

format' = formatTime defaultTimeLocale . iso8601DateFormat
dateStr = format' Nothing
timeStr = format' (Just "%T  ")

-- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
