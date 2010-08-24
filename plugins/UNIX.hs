import Buster.Plugin
import Control.Monad
import Control.Monad.Trans
import Data.List
import Data.Time.Clock
import System.Process

main = do uptime <- uptime `fmap` getCurrentTime
          pluginMain $ commandPlugin [("uptime", uptime), ("quit", quit),
                                      ("loadAverage", loadAverage)]
  where
    uptime zero _ = do
      now <- liftIO getCurrentTime
      respond ("Uptime: " ++ niceTime (diffUTCTime now zero) "")

    -- TODO: Remote termination & permissions system
    quit _ = respond "I'll never stop."

    loadAverage _ = do u <- liftIO $ readProcess "uptime" [] ""
                       let Just l = isPrefixOf "average" `find` tails u
                       respond $ unwords $ words (drop 9 l)
 
niceTime td = (++) . join . intersperse " " . filter (not . null) . map f $
      [(years      , "y"), (months % 12, "m"),
       (days   % 28, "d"), (hours  % 24, "h"),
       (mins   % 60, "m"), (secs   % 60, "s")]
    where
      secs    = abs (round td); mins   = secs   // 60
      hours   = mins // 60    ; days   = hours  // 24
      months  = days // 28    ; years  = months // 12
      (//)    = div           ; (%)    = mod
      f (i,s) | i == 0    = []
              | otherwise = show i ++ s

-- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
