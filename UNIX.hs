module UNIX (unixPlugin) where

import Control.Monad.State
import Data.List
import System.Exit
import System.Time
import Misc
import IRC

io = liftIO :: IO a -> InChan a

unixPlugin :: IO Plugin
unixPlugin = do uptime <- uptime `fmap` getClockTime
                return $ Plugin "UNIX" [("uptime", uptime), ("quit", quit)]
                                Nothing
  where
    uptime zero _ = do
      now <- io getClockTime
      let diff = pretty (diffClockTimes now zero) ""
      chanMsg $ "Uptime: " ++ diff

    quit _ = do lift $ write "QUIT" ["Exiting"]
                io $ exitWith ExitSuccess
 
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
