module Buster.Machine.Logging (plugin) where

import Buster.IRC
import Buster.Misc
import Control.Monad.State
import Data.Time
import System.Locale

io = liftIO :: IO a -> Net a

plugin = makePlugin $ return $ Plugin "Logging" [] (Just logger)
 
logger msg = io $ do
    time <- getZonedTime
    putStrLn $ pretty time . ("  " ++) . pretty msg $ ""

instance Pretty ZonedTime where
    pretty = showString . formatTime defaultTimeLocale
                                     (iso8601DateFormat (Just "%T"))
