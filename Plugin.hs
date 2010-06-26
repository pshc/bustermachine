module Plugin where
 
import Data.List
import Control.Monad.State
import System.IO
import System.Time
import Misc

data Response = ChannelMessage String

data Plugin = Plugin {
    pluginName :: String,
    pluginCommands :: [(String, String -> IO [Response])]
}

resource :: IO [Plugin]
resource = do uptime <- setupUptime
              return [Plugin "UNIX" [("uptime", uptime)]]

setupUptime = uptime `fmap` getClockTime
  where
    uptime zero _ = do
      now <- liftIO getClockTime
      return [ChannelMessage (pretty (diffClockTimes now zero) "")]
 
instance Pretty TimeDiff where
  pretty td = (++) . join . intersperse " " . filter (not . null) . map f $
      [(years          ,"y") ,(months `mod` 12,"m")
      ,(days   `mod` 28,"d") ,(hours  `mod` 24,"h")
      ,(mins   `mod` 60,"m") ,(secs   `mod` 60,"s")]
    where
      secs    = abs $ tdSec td  ; mins   = secs   `div` 60
      hours   = mins   `div` 60 ; days   = hours  `div` 24
      months  = days   `div` 28 ; years  = months `div` 12
      f (i,s) | i == 0    = []
              | otherwise = show i ++ s
 
-- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
