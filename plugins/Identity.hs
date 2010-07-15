
import Buster.Message
import Buster.Plugin
import Data.List

main = pluginMain $ processorPlugin noticer

noticer (Name "NickServ" _ _, Notice (Nick _) m)
  | "/msg NickServ IDENTIFY" `isInfixOf` m = do
    let pass = Nothing -- TODO: pass <- getConfig "NickServPassword"
    case pass of Just p  -> respondChat (Nick "NickServ") ("identify " ++ p)
                 Nothing -> io $ putStrLn "Need 'NickServPassword' config!"
  | "Password accepted" `isInfixOf` m = io $ putStrLn "Identified self."
noticer _ = return ()
