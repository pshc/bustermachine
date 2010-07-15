import Buster.Misc
import Buster.Plugin
import Control.Monad
import Data.List
import Data.Time.Clock

main = do uptime <- uptime `fmap` getCurrentTime
          pluginMain $ commandPlugin [("uptime", uptime), ("quit", quit)]
  where
    uptime zero ch _ = do
      now <- io getCurrentTime
      let diff = pretty (diffUTCTime now zero) ""
      respondChat ch ("Uptime: " ++ diff)

    -- TODO: Remote termination & permissions system
    quit ch _ = respondChat ch "I'll never stop."
 
instance Pretty NominalDiffTime where
  pretty td = (++) . join . intersperse " " . filter (not . null) . map f $
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
