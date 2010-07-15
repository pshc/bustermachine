{-# LANGUAGE RecordWildCards #-}
module Buster.Plugin (Machine, Plugin,
       commandPlugin, pluginMain, processorPlugin,
       io, lookupConfig, respondChat,
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

type Plugin = ReaderT PluginState IO

commandPlugin cmds = PluginImpl (Map.fromList cmds) Nothing
processorPlugin p  = PluginImpl (Map.empty) (Just p)

pluginMain :: PluginImpl -> IO ()
pluginMain (PluginImpl {..}) = do
    [r, w] <- mapM fdToHandle =<< (map (Fd . read) `fmap` getArgs)
    mapM_ dontBuffer [stdout, stderr, r, w]
    send w $ API (Map.keysSet pluginCommands) (isJust pluginProcessor)
    forever loop `runReaderT` PluginState r w
  where
    loop = recv' >>= either invalidReq doReq

    doReq (ReqProcess msg)    = check ($ msg) pluginProcessor
    doReq (ReqCommand t ch a) = check ($ (ch, a)) (uncurry `fmap`
                                      Map.lookup t pluginCommands)
    check f = maybe (io $ putStrLn "Request unsupported")
                    ((>> send' EndResponse) . f)

respondChat :: Target -> String -> Plugin ()
respondChat t s = send' $ ClientMsg $ PrivMsg t (Chat s)

lookupConfig :: String -> Plugin (Maybe String)
lookupConfig s = do send' $ ConfigLookup s
                    recv' >>= either ((>> return Nothing) . invalidReq) gotIt
  where
    gotIt (ReqConfig k (Just v)) | k == s = return (Just v)
    gotIt _                               = return Nothing

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

-- MACHINE-SIDE INTERNALS

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
                  | ReqConfig String (Maybe String)
                  deriving (Read, Show)

dispatchPlugins :: IORef Machine -> MessageProcessor
dispatchPlugins mRef msg = do
    mach@(Machine {..}) <- liftIO $ readIORef mRef
    mapM_ doProc [p | p <- Map.elems machPlugins, apiHasProcessor (ipcAPI p)]
    case snd msg of
      PrivMsg t (Chat s@('!':_)) -> eval machPlugins t s
      _                          -> return ()
  where
    doProc p = do send (ipcWrite p) (ReqProcess msg)
                  handleResponses p

eval ps t ('!':s) = case words s of
    (w:_) -> do case Map.fold (collectPlugins $ map toLower w) [] ps of
                  []  -> privMsg t ("No such command " ++ w ++ ".")
                  [p] -> do let arg = stripLeft $ drop (length w) s
                            send (ipcWrite p) (ReqCommand w t arg)
                            handleResponses p
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

handleResponses :: PluginIPC -> Net ()
handleResponses (PluginIPC {..}) = handleOne
  where
    handleOne = recv ipcRead >>= either invalidResp go

    go EndResponse = return ()
    go resp        = doResponse ipcWrite resp >> handleOne

doResponse _ (ClientMsg msg) = case msg of
    PrivMsg t msg -> privMsg t $ formatChat msg
    _             -> liftIO $ putStrLn $ "TODO: Perform " ++ show msg
  where
    formatChat (Chat s)   = s
    formatChat (Action s) = "\001ACTION " ++ s ++ "\001"

doResponse w (ConfigLookup k) = do v <- getConfig k
                                   liftIO $ send w $ ReqConfig k v

privMsg :: Target -> String -> Net ()
privMsg t s = write "PRIVMSG" [pretty t "", s]

invalidResp = liftIO . putStrLn . ("Invalid resp: " ++)

-- PLUGIN-SIDE INTERNALS

data PluginImpl = PluginImpl {
    pluginCommands :: Map String (Target -> String -> Plugin ()),
    pluginProcessor :: Maybe (ServerMsg -> Plugin ())
}

data PluginState = PluginState {
    pluginRead, pluginWrite :: Handle
}

data PluginResp = ClientMsg IRCMsg | ConfigLookup String | EndResponse
                  deriving (Read, Show)

send' :: PluginResp -> Plugin ()
send' = (asks pluginWrite >>=) . flip send

recv' :: Plugin (Either String MachineReq)
recv' = asks pluginRead >>= recv

invalidReq = io . putStrLn . ("Invalid req: " ++)

 -- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
