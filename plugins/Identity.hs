
import Buster.Message
import Buster.Plugin
import Data.List

main = pluginMain $ processorPlugin noticer

noticer (Name "NickServ" _ _, Notice (Nick _) m)
  | "/msg NickServ IDENTIFY" `isInfixOf` m = do
    let pass = Nothing -- TODO: pass <- getConfig "NickServPassword"
    case pass of Just p  -> returnChat (Nick "NickServ") ("identify " ++ p)
                 Nothing -> do putStrLn "Need 'NickServPassword' config!"
                               noResponse
  | "Password accepted" `isInfixOf` m = do putStrLn "Identified self."
                                           noResponse
noticer _ = noResponse
