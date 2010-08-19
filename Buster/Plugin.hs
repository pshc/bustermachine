{-# LANGUAGE DeriveDataTypeable, RecordWildCards, TypeSynonymInstances,
             ViewPatterns #-}
module Buster.Plugin (Machine, Plugin,
       commandPlugin, hybridPlugin, pluginMain, processorPlugin,
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
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable
import Prelude hiding (catch)
import System.IO
import System.Posix

-- FOR PLUGIN USE

type Plugin = ReaderT PluginState IO

commandPlugin cmds  = PluginImpl (Map.fromList cmds) Nothing
processorPlugin p   = PluginImpl (Map.empty) (Just p)
hybridPlugin cmds p = PluginImpl (Map.fromList cmds) (Just p)

pluginMain :: PluginImpl -> IO ()
pluginMain (PluginImpl {..}) = do
    [r, w] <- mapM parsePipe ["READ", "WRITE"]
    mapM_ dontBuffer [stdout, stderr, r, w]
    send w $ API (Map.keysSet pluginCommands) (isJust pluginProcessor)
    catch (loop `runReaderT` PluginState r w) handleError
    hClose r; hClose w
  where
    loop = recv' >>= either invalidReq ((>> loop) . doReq)

    doReq (ReqProcess msg)             = check ($ msg) pluginProcessor
    doReq (ReqCommand cmd src ch args) =
        check (\f -> f src ch args) (lower cmd `Map.lookup` pluginCommands)

    check f = maybe (io $ throwIO ReqUnsupportedException)
                    ((>> send' EndResponse) . f)

    parsePipe = (>>= fdToHandle) . fmap (Fd . read . fromJust) . getEnv

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

instance Context Plugin where
  contextLookup user = do send' (UserLookup user)
                          recv' >>= either invalidReq gotIt
    where
      gotIt (ReqUser u ui) | u == user = return ui
      gotIt _                          = return Nothing

io = liftIO :: IO a -> Plugin a

-- FOR NONO USE

setupMachine :: [String] -> IO MessageProcessor
setupMachine ps = do installHandler openEndedPipe Ignore Nothing
                     getProcessID >>= createProcessGroup
                     r <- newIORef (Machine Map.empty)
                     installHandler processStatusChanged
                                    (Catch $ cleanupZombies r) Nothing
                     mapM_ (loadPlugin r) ps
                     return $ dispatchPlugins r
  where
    cleanupZombies r = do mach@(Machine {..}) <- readIORef r
                          m' <- cleanups (Map.assocs machPlugins) machPlugins
                          writeIORef r $ mach { machPlugins = m' }

    cleanups []           m = return m
    cleanups ((nm, p):ps) m = getProcessStatus False True (ipcPID p)
                              >>= maybe (cleanups ps m) handleStatus
      where
        handleStatus (Exited n)     = unload $ "exit (" ++ show n ++ ")"
        handleStatus (Terminated s) = unload $ "termination (" ++ show s ++ ")"
        handleStatus (Stopped _)    = do killPlugin p
                                         unload "stoppage (now terminated)"
        unload s = do when (nm `Map.member` m) $
                       putStrLn $ "Unloading " ++ nm ++ " due to process " ++ s
                      cleanups ps (nm `Map.delete` m)

data Machine = Machine {
   machPlugins :: Map String PluginIPC
}

loadPlugin :: IORef Machine -> String -> IO Bool
loadPlugin ref nm = do
    mach@(Machine {..}) <- readIORef ref
    exists <- checkExists
    if not exists then return False else
       if nm `Map.member` machPlugins
         then putStrLn (nm ++ " is already loaded!") >> return False
         else doLoad >>= maybe (return False) (addAPI mach)
  where
    doLoad = do (pr, Fd cw) <- createPipe
                (Fd cr, pw) <- createPipe
                gid <- getProcessGroupID
                let env = [("READ", show cr), ("WRITE", show cw)]
                    bin = dir ++ "/" ++ nm
                pid <- forkProcess $ do joinProcessGroup gid
                                        executeFile bin False [] (Just env)
                r <- fdToHandle pr; dontBuffer r
                w <- fdToHandle pw; dontBuffer w
                recv r >>= either (badAPI r w pid)
                                  (return . Just . PluginIPC pid r w)

    badAPI r w pid l = do putStrLn $ "Got invalid spec from " ++ nm ++ ":"
                          putStrLn l
                          hClose r; hClose w
                          signalProcess softwareTermination pid
                          return Nothing

    addAPI m a = do let m' = Map.insert nm a (machPlugins m)
                    writeIORef ref $ m { machPlugins = m' }
                    return True

    dir = "plugins/bin/"
    checkExists = bracket (openDirStream dir) closeDirStream go
      where
        go s = do fp <- readDirStream s
                  case fp of "" -> return False
                             "." -> go s; ".." -> go s
                             _ | fp == nm  -> return True
                               | otherwise -> go s

unloadPlugin r nm = do mach <- readIORef r
                       maybe (return ()) (go mach)
                             (nm `Map.lookup` machPlugins mach)
  where
    go m p = do writeIORef r $ m {machPlugins = nm `Map.delete` machPlugins m}
                killPlugin p
                --- XXX: Could block indefinitely from a malicious plugin
                getProcessStatus True True (ipcPID p)
                return ()

killPlugin = signalProcess softwareTermination . ipcPID

data PluginIPC = PluginIPC {
    ipcPID :: ProcessID,
    ipcRead, ipcWrite :: Handle,
    ipcAPI :: API
}

data API = API {
    apiCommandSet :: Set String,
    apiHasProcessor :: Bool
} deriving (Read, Show)

data MachineReq = ReqProcess ServerMsg | ReqCommand String User Target String
                  | ReqConfig String (Maybe String)
                  | ReqUsers Chan (Maybe (Map User Priv))
                  | ReqChans (Map Chan ChannelState)
                  | ReqUser User (Maybe UserInfo)
                  deriving (Read, Show)

type Respond = ReaderT Target Users
respond :: String -> Respond ()
respond msg = ask >>= (lift . flip privMsg msg)

dispatchPlugins :: IORef Machine -> MessageProcessor
dispatchPlugins mRef (src, msg) = do
    Machine { machPlugins = ps } <- liftIO $ readIORef mRef
    mapM_ doProc [p | p <- Map.elems ps, apiHasProcessor (ipcAPI p)]
    case msg of
      PrivMsg t (Chat ('!':s)) -> do respondee <- reflect t
                                     runReaderT (doCmd ps t s (words s))
                                                respondee
      _                        -> return ()
  where
    reflect (Chan ch) = return $ Chan ch
    reflect _         = return $ User src

    doProc p = do send (ipcWrite p) (ReqProcess (src, msg))
                  handleResponses p

    doCmd ps _ _ ((lower -> c):ws)
      | c `elem` builtinCmds = builtinCmd ps mRef c ws
    doCmd ps t s ((lower -> c):_) = case Map.fold (collectPlugins c) [] ps of
        []  -> respond ("No such command " ++ c ++ ".")
        [p] -> lift $ do let args = stripLeft $ drop (length c) s
                         send (ipcWrite p) (ReqCommand c src t args)
                         handleResponses p
        ps  -> respond ("Ambiguous command " ++ c ++ ".") -- TODO
      where
        collectPlugins w p cs | w `Set.member` apiCommandSet (ipcAPI p) = p:cs
                              | otherwise                               = cs
    doCmd _ _ _ []           = respond "Command expected."

builtinCmds = ["list", "load", "reload", "unload"]
builtinCmd ps _ "list" [] =
    let ps' = Set.toList $ Set.insert "Plugins" (Map.keysSet ps)
    in respond (intercalate ", " ps')
builtinCmd _ _ cmd ["Plugins"] = respond $ case cmd of -- fake meta plugin
    "list"   -> showCmdList builtinCmds
    "load"   -> "Plugins is already loaded."
    "reload" -> "Cannot reload Plugins."
    "unload" -> "Cannot unload Plugins."
builtinCmd ps r c [nm] = case (c, nm `Map.lookup` ps) of
    ("list", Just p)   -> respond $ ipcCmdList p
    ("load", Nothing)  -> loadIt
    ("load", Just _)   -> respond (nm ++ " is already loaded.")
    ("reload", Just _) -> liftIO (unloadPlugin r nm) >> loadIt
    ("unload", Just _) -> liftIO (unloadPlugin r nm) >> gotcha
    _                  -> respond $ "No such plugin '" ++ nm ++ ".'"
  where
    loadIt = do r <- liftIO $ loadPlugin r nm
                if r then gotcha else respond "Plugin load failed."
    gotcha = respond "Gotcha."
    ipcCmdList = showCmdList . Set.toList . apiCommandSet . ipcAPI

builtinCmd _ _ _ _ = respond "Plugin expected."

showCmdList []   = "That plugin has no commands."
showCmdList cmds = (intercalate ", " cmds)

send :: (MonadIO m, Show a) => Handle -> a -> m ()
send h = liftIO . (>> hFlush h) . hPutStrLn h . show

recv :: (MonadIO m, Read a) => Handle -> m (Either String a)
recv h = do l <- liftIO $ hGetLine h
            return $ case reads l of [(a, "")] -> Right a
                                     _         -> Left l

handleResponses :: PluginIPC -> Users ()
handleResponses p@(PluginIPC {..}) = handleOne
  where
    handleOne = recv ipcRead >>= either (liftIO . invalidIPC) go

    go EndResponse = return ()
    go resp        = doResponse ipcWrite resp >> handleOne

    invalidIPC l = do putStrLn $ "Got invalid resp: " ++ l
                      killPlugin p

doResponse w q = case q of
    ClientMsg (PrivMsg t msg) -> privMsg t $ formatChat msg
    ClientMsg m               -> liftIO $ putStrLn $ "TODO: Perform " ++ show m

    ConfigLookup k -> do v <- lift (getConfig k)
                         reply $ ReqConfig k v
    UsersQuery ch  -> lift (getChan ch) >>= reply . ReqUsers ch
                                                  . fmap chanUsers
    ChansQuery     -> lift getChans >>= reply . ReqChans
    UserLookup u   -> lookupUser u >>= reply . ReqUser u
  where
    formatChat (Chat s)   = s
    formatChat (Action s) = "\001ACTION " ++ s ++ "\001"
    reply = liftIO . send w

privMsg :: Target -> String -> Users ()
privMsg t s = do dest <- pretty t
                 lift $ write "PRIVMSG" [dest "", s]

-- PLUGIN-SIDE INTERNALS

data PluginImpl = PluginImpl {
    pluginCommands :: Map String (User -> Target -> String -> Plugin ()),
    pluginProcessor :: Maybe (ServerMsg -> Plugin ())
}

data PluginState = PluginState {
    pluginRead, pluginWrite :: Handle
}

data PluginResp = ClientMsg IRCMsg | ConfigLookup String | UsersQuery Chan
                  | ChansQuery | UserLookup User | EndResponse
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
