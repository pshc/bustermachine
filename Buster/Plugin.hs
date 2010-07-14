{-# LANGUAGE RecordWildCards #-}
module Buster.Plugin (Machine(..), Plugin(..), PluginResp(..),
       commandPlugin, noResponse, pluginMain, processorPlugin, returnChat,
       loadPlugin, setupMachine,
       ) where

import Buster.IRC
import Buster.Message
import Buster.Misc
import Control.Monad.State
import Data.Char
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment
import System.IO
import System.Posix

-- FOR PLUGIN USE

data Plugin = Plugin {
    pluginCommands :: Map String (Target -> String -> IO (Maybe PluginResp)),
    pluginProcessor :: Maybe (ServerMsg -> IO (Maybe PluginResp))
}

data PluginResp = ClientMsg IRCMsg deriving (Read, Show)

commandPlugin cmds = Plugin (Map.fromList cmds) Nothing
processorPlugin p  = Plugin (Map.empty) (Just p)

pluginMain :: Plugin -> IO ()
pluginMain (Plugin {..}) = do 
    [r, w] <- mapM fdToHandle =<< (map (Fd . read) `fmap` getArgs)
    mapM_ dontBuffer [stdout, stderr, r, w]
    putStrLn "done."
    send w api
    forever (recv r >>= either (putStrLn . ("Invalid req: " ++)) (doReq w))
  where
    api = API (Map.keysSet pluginCommands) (isJust pluginProcessor)

    doReq w (ReqProcess msg)    = check w ($ msg) pluginProcessor
    doReq w (ReqCommand t ch a) = check w ($ (ch, a)) (uncurry `fmap`
                                        Map.lookup t pluginCommands)
    check w f = maybe (putStrLn "Request unsupported") ((>>= respond w) . f)
    respond :: Handle -> Maybe PluginResp -> IO ()
    respond w = send w

returnChat t s = return $ Just $ ClientMsg $ PrivMsg t (Chat s)

noResponse = return Nothing :: IO (Maybe PluginResp)

-- FOR NONO USE

setupMachine :: [String] -> IO MessageProcessor
setupMachine ps = do installHandler openEndedPipe Ignore Nothing
                     m <- newIORef =<< foldM loadPlugin (Machine Map.empty) ps
                     return $ dispatchPlugins m

data Machine = Machine {
   machPlugins :: Map String PluginIPC
}

loadPlugin :: Machine -> String -> IO Machine
loadPlugin mach@(Machine {..}) nm
    | nm `Map.member` machPlugins = do putStrLn $ nm ++ " is already loaded!"
                                       return mach
    | otherwise = do
        (Fd cr, pw) <- createPipe
        (pr, Fd cw) <- createPipe
        putStr $ "Loading " ++ nm ++ "... "
        pid <- forkProcess $ executeFile ("plugins/bin/" ++ nm) False
                                         [show cr, show cw] Nothing
        r <- fdToHandle pr; dontBuffer r
        w <- fdToHandle pw; dontBuffer w
        recv r >>= either badAPI (addAPI . PluginIPC pid r w)
  where
    badAPI _ = do putStrLn "Invalid plugin interface spec!"
                  return mach
    addAPI api = return $ mach { machPlugins = Map.insert nm api machPlugins }
-- FOR INTERNAL USE

data PluginIPC = PluginIPC {
    ipcPID :: ProcessID,
    ipcRead, ipcWrite :: Handle,
    ipcAPI :: API
}

data API = API {
    apiCommandSet :: Set String,
    apiHasProcessor :: Bool
} deriving (Read, Show)

data MachineReq = ReqProcess ServerMsg | ReqCommand String Target String
                  deriving (Read, Show)

io = liftIO :: IO a -> Net a

dispatchPlugins :: IORef Machine -> MessageProcessor
dispatchPlugins mRef msg = do
    mach@(Machine {..}) <- io $ readIORef mRef
    mapM_ doProc [p | p <- Map.elems machPlugins, apiHasProcessor (ipcAPI p)]
    case snd msg of
      PrivMsg t (Chat s@('!':_)) -> eval machPlugins t s
      _                          -> return ()
  where
    doProc (PluginIPC {..}) = do io $ send ipcWrite (ReqProcess msg)
                                 io (recv ipcRead) >>= doResponse'

eval ps t ('!':s) = case words s of
    (w:_) -> do case Map.fold (collectPlugins $ map toLower w) [] ps of
                  []  -> privMsg t ("No such command " ++ w ++ ".")
                  [p] -> do let arg = stripLeft $ drop (length w) s
                            io $ send (ipcWrite p) (ReqCommand w t arg)
                            io (recv $ ipcRead p) >>= doResponse'
                  ps  -> privMsg t ("Ambiguous command " ++ w ++ ".") -- TODO
    []    -> privMsg t "Command expected."
  where
    collectPlugins w p cs | w `Set.member` apiCommandSet (ipcAPI p) = p:cs
                          | otherwise                               = cs
eval _ _ _ = return ()

send :: (Show a) => Handle -> a -> IO ()
send h = (>> hFlush h) . hPutStrLn h . show

recv :: (Read a) => Handle -> IO (Either String a)
recv h = do l <- hGetLine h
            return $ case reads l of [(a, "")] -> Right a
                                     _         -> Left l

doResponse (ClientMsg msg) = case msg of
    PrivMsg t msg -> privMsg t $ formatChat msg
    _             -> io $ putStrLn $ "TODO: Perform " ++ show msg
  where
    formatChat (Chat s)   = s
    formatChat (Action s) = "\001ACTION " ++ s ++ "\001"

doResponse' :: Either String (Maybe PluginResp) -> Net ()
doResponse' = either (io . putStrLn . ("Got invalid response: " ++))
                     (maybe (return ()) doResponse)

privMsg :: Target -> String -> Net ()
privMsg t s = write "PRIVMSG" [pretty t "", s]

 -- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
