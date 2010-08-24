
import Buster.Message
import Buster.Plugin
import Control.Monad.Trans
import Data.List

main = pluginMain $ processorPlugin noticer

noticer (u, Notice (User _) m) = if isNickServ u then nickServ u m
                                                 else return ()
  where
    isNickServ = const False -- TODO
noticer _ = return ()

io = liftIO

nickServ ns m
  | "/msg NickServ IDENTIFY" `isInfixOf` m = do
    pass <- lookupConfig "NickServPassword"
    case pass of Just p  -> sendChat (User ns) ("identify " ++ p)
                 Nothing -> io $ putStrLn "Need 'NickServPassword' config!"
  | "Password accepted" `isInfixOf` m = io $ putStrLn "Identified self."
  | "Password incorrect" `isInfixOf` m = io $ putStrLn "Bad NickServ password."
  | otherwise = return ()
