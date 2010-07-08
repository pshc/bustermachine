module Buster.Machine.Identity (plugin) where

import Buster.IRC
import Buster.Message
import Buster.Plugin
import Control.Monad.State
import Data.List

io = liftIO :: IO a -> Net a

plugin = makePlugin $ return $ processorPlugin noticer

noticer (Name "NickServ" _ _, Notice (Nick _) m)
  | "/msg NickServ IDENTIFY" `isInfixOf` m = do
    pass <- getConfig "NickServPassword"
    case pass of Just pass -> write "PRIVMSG" ["NickServ", "identify " ++ pass]
                 Nothing   -> io $ putStrLn "Need 'NickServPassword' config!"
  | "Password accepted" `isInfixOf` m = do
    io $ putStrLn "Identified self."
noticer _ = return ()
