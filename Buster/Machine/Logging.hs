module Buster.Machine.Logging (plugin) where

import Buster.IRC
import Buster.Misc
import Control.Monad.State

io = liftIO :: IO a -> Net a

plugin = makePlugin $ return $ Plugin "Logging" [] (Just logger)
 
logger msg = io $ putStrLn (pretty msg "")
