{-# LANGUAGE RecordWildCards #-}
module Buster.Plugin (
       commandPlugin, io, pluginMain, processorPlugin, respondChat,
       loadPlugin, setupMachine,
       ) where

import Buster.IRC
import Buster.Message
import Buster.Misc
import Control.Monad.Reader
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

data PluginImpl = PluginImpl {
    pluginCommands :: Map String (Target -> String -> Plugin ()),
    pluginProcessor :: Maybe (ServerMsg -> Plugin ())
}

type Plugin = ReaderT PluginState IO
data PluginState = PluginState {
    pluginRead, pluginWrite :: Handle
}

data PluginResp = ClientMsg IRCMsg | EndResponse deriving (Read, Show)

commandPlugin cmds = PluginImpl (Map.fromList cmds) Nothing
processorPlugin p  = PluginImpl (Map.empty) (Just p)

pluginMain :: PluginImpl -> IO ()
pluginMain (PluginImpl {..}) = do
    [r, w] <- mapM fdToHandle =<< (map (Fd . read) `fmap` getArgs)
    mapM_ dontBuffer [stdout, stderr, r, w]
    send w api
    let st = PluginState r w
    forever (recv r >>= either invalidReq doReq) `runReaderT` st
  where
    api = API (Map.keysSet pluginCommands) (isJust pluginProcessor)

    doReq (ReqProcess msg)    = check ($ msg) pluginProcessor
    doReq (ReqCommand t ch a) = check ($ (ch, a)) (uncurry `fmap`
                                      Map.lookup t pluginCommands)
    check f = maybe (io $ putStrLn "Request unsupported") $ \req -> do
      f req
      w <- asks pluginWrite
      send w EndResponse

    invalidReq = io . putStrLn . ("Invalid req: " ++)

respondChat :: Target -> String -> Plugin ()
respondChat t s = do w <- asks pluginWrite
                     send w $ ClientMsg $ PrivMsg t (Chat s)

io = liftIO :: IO a -> Plugin a

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
        (pr, Fd cw) <- createPipe
        (Fd cr, pw) <- createPipe
        putStr $ "Loading " ++ nm ++ "... "
        pid <- forkProcess $ executeFile ("plugins/bin/" ++ nm) False
                                         [show cr, show cw] Nothing
        r <- fdToHandle pr; dontBuffer r
        w <- fdToHandle pw; dontBuffer w
        recv r >>= either badAPI (addAPI . PluginIPC pid r w)
  where
    badAPI _ = do putStrLn "invalid spec!"
                  return mach
    addAPI a = do putStrLn "done."
                  return $ mach { machPlugins = Map.insert nm a machPlugins }

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

dispatchPlugins :: IORef Machine -> MessageProcessor
dispatchPlugins mRef msg = do
    mach@(Machine {..}) <- liftIO $ readIORef mRef
    mapM_ doProc [p | p <- Map.elems machPlugins, apiHasProcessor (ipcAPI p)]
    case snd msg of
      PrivMsg t (Chat s@('!':_)) -> eval machPlugins t s
      _                          -> return ()
  where
    doProc (PluginIPC {..}) = do send ipcWrite (ReqProcess msg)
                                 recv ipcRead >>= doResponse'

eval ps t ('!':s) = case words s of
    (w:_) -> do case Map.fold (collectPlugins $ map toLower w) [] ps of
                  []  -> privMsg t ("No such command " ++ w ++ ".")
                  [p] -> do let arg = stripLeft $ drop (length w) s
                            send (ipcWrite p) (ReqCommand w t arg)
                            recv (ipcRead p) >>= doResponse'
                  ps  -> privMsg t ("Ambiguous command " ++ w ++ ".") -- TODO
    []    -> privMsg t "Command expected."
  where
    collectPlugins w p cs | w `Set.member` apiCommandSet (ipcAPI p) = p:cs
                          | otherwise                               = cs
eval _ _ _ = return ()

send :: (MonadIO m, Show a) => Handle -> a -> m ()
send h = liftIO . (>> hFlush h) . hPutStrLn h . show

recv :: (MonadIO m, Read a) => Handle -> m (Either String a)
recv h = do l <- liftIO $ hGetLine h
            return $ case reads l of [(a, "")] -> Right a
                                     _         -> Left l

doResponse (ClientMsg msg) = case msg of
    PrivMsg t msg -> privMsg t $ formatChat msg
    _             -> liftIO $ putStrLn $ "TODO: Perform " ++ show msg
  where
    formatChat (Chat s)   = s
    formatChat (Action s) = "\001ACTION " ++ s ++ "\001"

doResponse' :: Either String (Maybe PluginResp) -> Net ()
doResponse' = either (liftIO . putStrLn . ("Got invalid response: " ++))
                     (maybe (return ()) doResponse)

privMsg :: Target -> String -> Net ()
privMsg t s = write "PRIVMSG" [pretty t "", s]

 -- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
