module Buster.Machine.UNIX (plugin) where

import Buster.IRC
import Buster.Misc
import Control.Monad.State
import Data.List
import Data.Time.Clock
import System.Exit

io = liftIO :: IO a -> InChan a

plugin = makePlugin $ do
    uptime <- uptime `fmap` getCurrentTime
    return $ Plugin "UNIX" [("uptime", uptime), ("quit", quit)] Nothing
  where
    uptime zero _ = do
      now <- io getCurrentTime
      let diff = pretty (diffUTCTime now zero) ""
      chanMsg $ "Uptime: " ++ diff

    quit _ = do lift $ write "QUIT" ["Exiting"]
                io $ exitWith ExitSuccess
 
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
