module Logging (loggingPlugin) where

import Control.Monad.State
import Misc
import IRC

io = liftIO :: IO a -> Net a

loggingPlugin :: IO Plugin
loggingPlugin = return $ Plugin "Logging" [] (Just logger)
 
logger msg = io $ putStrLn (pretty msg "")
