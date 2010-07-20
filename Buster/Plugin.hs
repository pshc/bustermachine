{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Buster.Plugin (Machine, Plugin,
       commandPlugin, pluginMain, processorPlugin,
       io, lookupConfig, queryChans, queryUsers, respondChat,
       setupMachine
       ) where

import Buster.IRC
import Buster.Message
import Buster.Misc
import Control.Exception
import Control.Monad.Reader
import Data.Char
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable
import Prelude hiding (catch)
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
    catch (loop `runReaderT` PluginState r w) handleError
    hClose r; hClose w
  where
    loop = recv' >>= either invalidReq ((>> loop) . doReq)

    doReq (ReqProcess msg)    = check ($ msg) pluginProcessor
    doReq (ReqCommand t ch a) = check ($ (ch, a)) (uncurry `fmap`
                                      Map.lookup t pluginCommands)
    check f = maybe (io $ throwIO ReqUnsupportedException)
                    ((>> send' EndResponse) . f)

    handleError :: ReqException -> IO ()
    handleError = print -- and leave loop

respondChat :: Target -> String -> Plugin ()
respondChat t s = send' $ ClientMsg $ PrivMsg t (Chat s)

lookupConfig :: String -> Plugin (Maybe String)
lookupConfig s = do send' $ ConfigLookup s
                    recv' >>= either invalidReq gotIt
  where
    gotIt (ReqConfig k (Just v)) | k == s = return (Just v)
    gotIt _                               = return Nothing

queryChans :: Plugin (Map Chan ChannelState)
queryChans = do send' ChansQuery
                recv' >>= either invalidReq (\(ReqChans c) -> return c)

queryUsers :: Chan -> Plugin (Maybe (Map User Priv))
queryUsers ch = do send' $ UsersQuery ch
                   recv' >>= either invalidReq gotIt
  where
    gotIt (ReqUsers c m) | c == ch = return m
    gotIt _                        = return Nothing

io = liftIO :: IO a -> Plugin a

-- FOR NONO USE

setupMachine :: [String] -> IO MessageProcessor
setupMachine ps = do installHandler openEndedPipe Ignore Nothing
                     getProcessID >>= createProcessGroup
                     r <- newIORef (Machine Map.empty)
                     installHandler processStatusChanged
                                    (Catch $ cleanupZombies r) Nothing
                     mapM_ (addPlugin r) ps
                     return $ dispatchPlugins r
  where
    cleanupZombies r = do mach@(Machine {..}) <- readIORef r
                          m' <- cleanups (Map.assocs machPlugins) machPlugins
                          writeIORef r $ mach { machPlugins = m' }

    cleanups []           m = return m
    cleanups ((nm, p):ps) m = getProcessStatus False True (ipcPID p)
                              >>= maybe (cleanups ps m) handleStatus
      where
        handleStatus (Exited n)     = unload $ "exit (code " ++ show n ++ ")"
        handleStatus (Terminated s) = unload $ "termination (" ++ show s ++ ")"
        handleStatus (Stopped _)    = do killPlugin p
                                         unload "stoppage (now terminated)"
        unload s = do putStrLn $ "Unloading " ++ nm ++ " due to process " ++ s
                      cleanups ps (nm `Map.delete` m)

data Machine = Machine {
   machPlugins :: Map String PluginIPC
}

addPlugin :: IORef Machine -> String -> IO ()
addPlugin ref nm = do
    putStr $ "Loading " ++ nm ++ "... "
    mach@(Machine {..}) <- readIORef ref
    if nm `Map.member` machPlugins then putStrLn $ nm ++ " is already loaded!"
       else loadPlugin nm >>= maybe (return ()) (addAPI mach ref)
  where
    addAPI m ref a = do putStrLn "done."
                        let m' = Map.insert nm a (machPlugins m)
                        writeIORef ref $ m { machPlugins = m' }

loadPlugin :: String -> IO (Maybe PluginIPC)
loadPlugin nm = do
    (pr, Fd cw) <- createPipe
    (Fd cr, pw) <- createPipe
    gid <- getProcessGroupID
    pid <- forkProcess $ do joinProcessGroup gid
                            executeFile ("plugins/bin/" ++ nm) False
                                        [show cr, show cw] Nothing
    r <- fdToHandle pr; dontBuffer r
    w <- fdToHandle pw; dontBuffer w
    recv r >>= either (badAPI r w pid) (return . Just . PluginIPC pid r w)
  where
    badAPI r w pid l = do putStrLn $ "Got invalid spec from " ++ nm ++ ":"
                          putStrLn l
                          hClose r; hClose w
                          signalProcess softwareTermination pid
                          return Nothing

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
                  | ReqUsers Chan (Maybe (Map User Priv))
                  | ReqChans (Map Chan ChannelState)
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
handleResponses p@(PluginIPC {..}) = handleOne
  where
    handleOne = recv ipcRead >>= either (liftIO . invalidIPC) go

    go EndResponse = return ()
    go resp        = doResponse ipcWrite resp >> handleOne

    invalidIPC l = do putStrLn $ "Got invalid resp: " ++ l
                      killPlugin p

doResponse _ (ClientMsg msg) = case msg of
    PrivMsg t msg -> privMsg t $ formatChat msg
    _             -> liftIO $ putStrLn $ "TODO: Perform " ++ show msg
  where
    formatChat (Chat s)   = s
    formatChat (Action s) = "\001ACTION " ++ s ++ "\001"

doResponse w (ConfigLookup k) = do v <- getConfig k
                                   liftIO $ send w $ ReqConfig k v
doResponse w (UsersQuery ch) = getChan ch >>= liftIO . send w . ReqUsers ch
                                                     . fmap chanUsers
doResponse w ChansQuery      = getChans >>= liftIO . send w . ReqChans

privMsg :: Target -> String -> Net ()
privMsg t s = write "PRIVMSG" [pretty t "", s]

killPlugin = signalProcess softwareTermination . ipcPID

-- PLUGIN-SIDE INTERNALS

data PluginImpl = PluginImpl {
    pluginCommands :: Map String (Target -> String -> Plugin ()),
    pluginProcessor :: Maybe (ServerMsg -> Plugin ())
}

data PluginState = PluginState {
    pluginRead, pluginWrite :: Handle
}

data PluginResp = ClientMsg IRCMsg | ConfigLookup String | UsersQuery Chan
                  | ChansQuery | EndResponse
                  deriving (Read, Show)

send' :: PluginResp -> Plugin ()
send' = (asks pluginWrite >>=) . flip send

recv' :: Plugin (Either String MachineReq)
recv' = asks pluginRead >>= recv

data ReqException = ReqParseException String | ReqUnsupportedException
                    deriving (Show, Typeable)
instance Exception ReqException

invalidReq = io . throwIO . ReqParseException

 -- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
