{-# LANGUAGE RecordWildCards, ViewPatterns #-}
module Buster.Machine (setupMachine) where

import Buster.Internal
import Buster.IRC
import Buster.Message
import Buster.Misc
import Control.Concurrent
import Control.Exception
import Control.Monad.Reader
import Control.Monad.State
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO
import System.Posix
import System.Timeout

setupMachine :: [String] -> IO MessageProcessor
setupMachine ps = do installHandler openEndedPipe Ignore Nothing
                     getProcessID >>= createProcessGroup
                     m <- newMVar (Machine Map.empty)
                     installHandler processStatusChanged
                                    (Catch $ modifyMVar_ m cleanupZombies)
                                    Nothing
                     mapM_ (modifyMVar m . loadPlugin) ps
                     return $ forkDispatch m
  where
    cleanupZombies mach@(Machine {..}) = do
        ps <- cleanups (Map.assocs machPlugins) machPlugins
        return $ mach { machPlugins = ps }

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

loadPlugin :: String -> Machine -> IO (Machine, Bool)
loadPlugin nm mach@(Machine {..}) = do
    exists <- checkExists
    if not exists then failure else
       if nm `Map.member` machPlugins
         then putStrLn (nm ++ " is already loaded!") >> failure
         else doLoad >>= maybe failure addAPI
  where
    failure = return (mach, False)

    doLoad = do (pr, Fd cw) <- createPipe
                (Fd cr, pw) <- createPipe
                gid <- getProcessGroupID
                let env = [("READ", show cr), ("WRITE", show cw)]
                    bin = dir ++ "/" ++ nm
                pid <- forkProcess $ do joinProcessGroup gid
                                        executeFile bin False [] (Just env)
                r <- fdToHandle pr; dontBuffer r
                w <- fdToHandle pw; dontBuffer w
                timeout (10*1000000) (recv r) >>= dealWithIt r w pid

    dealWithIt r w pid res = case res of
        Nothing        -> putStrLn ("Loading " ++ nm ++ " timed out.") >> nope
        Just (Left e)  -> do putStrLn $ "Got invalid spec from " ++ nm ++ ":"
                             putStrLn e
                             nope
        Just (Right a) -> do socks <- newMVar (r, w)
                             return $ Just (PluginIPC pid socks a)
      where
        nope = do hClose r; hClose w
                  signalProcess softwareTermination pid
                  return Nothing

    addAPI a = do let ps = Map.insert nm a machPlugins
                  return (mach { machPlugins = ps }, True)

    dir = "plugins/bin/"
    checkExists = bracket (openDirStream dir) closeDirStream go
      where
        go s = do fp <- readDirStream s
                  case fp of "" -> return False
                             "." -> go s; ".." -> go s
                             _ | fp == nm  -> return True
                               | otherwise -> go s

unloadPlugin nm mach = do maybe (return mach) go
                                (nm `Map.lookup` machPlugins mach)
  where
    go p = do let ps' = nm `Map.delete` machPlugins mach
              killPlugin p
              -- TODO: Kill the thread if this blocks for too long
              tid <- forkIO $ do getProcessStatus True True (ipcPID p)
                                 return ()
              return $ mach { machPlugins = ps' }

killPlugin = signalProcess softwareTermination . ipcPID

data PluginIPC = PluginIPC {
    ipcPID :: ProcessID,
    ipcSockets :: MVar (Handle, Handle),
    ipcAPI :: API
}

type RelayBack = ReaderT (Chan IRCMsg, Target) Users

relayBack msg = do (chan, t) <- ask
                   liftIO $ writeChan chan (PrivMsg t $ Chat msg)

relayMsg msg = do (chan, _) <- ask
                  liftIO $ writeChan chan msg

forkDispatch :: MVar Machine -> MessageProcessor
forkDispatch m chan msg = do
    f <- inception (dispatchPlugins m chan msg)
    liftIO $ forkIO $ do r <- timeout 5000000 f
                         when (isNothing r) $ putStrLn "Command timed out."
    return ()
  where
    inception f = do a <- lift get
                     b <- ask
                     return $ evalStateT (runReaderT f b) a

dispatchPlugins :: MVar Machine -> MessageProcessor
dispatchPlugins mv chan (src, msg) = do
    Machine { machPlugins = ps } <- liftIO $ readMVar mv
    mapM_ doProc [p | p <- Map.elems ps, apiHasProcessor (ipcAPI p)]
    case msg of
      PrivMsg t (Chat ('!':s)) -> runReaderT (doCmd ps t s (words s))
                                             (chan, reflect t)
      _                        -> return ()
  where
    reflect ch@(Channel _) = ch
    reflect _              = User src

    -- XXX: Better to use withMVar, but need inception
    doProc p = do (r, w) <- liftIO $ takeMVar (ipcSockets p)
                  send w (ReqProcess (src, msg))
                  handleResponses r w (ipcPID p) `runReaderT` (chan, User src)
                  liftIO $ putMVar (ipcSockets p) (r, w)

    doCmd ps _ _ ((lower -> c):ws)
      | c `elem` builtinCmds = builtinCmd ps mv c ws
    doCmd ps t s (c:_) = case Map.fold (collectPlugins $ IString c) [] ps of
        []  -> relayBack ("No such command " ++ c ++ ".")
        [p] -> do let args = stripLeft $ drop (length c) s
                  -- XXX: inception again
                  (r, w) <- liftIO $ takeMVar (ipcSockets p)
                  lift $ send w $ ReqCommand (IString c) src t args
                  handleResponses r w (ipcPID p)
                  liftIO $ putMVar (ipcSockets p) (r, w)
        ps  -> relayBack ("Ambiguous command " ++ c ++ ".") -- TODO
      where
        collectPlugins w p cs | w `Set.member` apiCommandSet (ipcAPI p) = p:cs
                              | otherwise                               = cs
    doCmd _ _ _ []           = relayBack "Command expected."

builtinCmds = ["list", "load", "reload", "unload"]
builtinCmd ps _ "list" [] =
    let ps' = Set.toList $ Set.insert "Plugins" (Map.keysSet ps)
    in relayBack (intercalate ", " ps')
builtinCmd _ _ cmd ["Plugins"] = relayBack $ case cmd of -- fake meta plugin
    "list"   -> showCmdList builtinCmds
    "load"   -> "Plugins is already loaded."
    "reload" -> "Cannot reload Plugins."
    "unload" -> "Cannot unload Plugins."
builtinCmd ps mv c [nm] = case (c, nm `Map.lookup` ps) of
    ("list", Just p)   -> relayBack $ ipcCmdList p
    ("load", Nothing)  -> loadIt
    ("load", Just _)   -> relayBack (nm ++ " is already loaded.")
    ("reload", Just _) -> liftIO (modifyMVar_ mv $ unloadPlugin nm) >> loadIt
    ("unload", Just _) -> liftIO (modifyMVar_ mv $ unloadPlugin nm) >> gotcha
    _                  -> relayBack $ "No such plugin '" ++ nm ++ ".'"
  where
    loadIt = do r <- liftIO $ modifyMVar mv (loadPlugin nm)
                if r then gotcha else relayBack "Plugin load failed."
    gotcha = relayBack "Gotcha."
    ipcCmdList = showCmdList . extract . Set.toList . apiCommandSet . ipcAPI
    extract = map (\(IString s) -> s)

builtinCmd _ _ _ _ = relayBack "Plugin expected."

showCmdList []   = "That plugin has no commands."
showCmdList cmds = (intercalate ", " cmds)

handleResponses r w pid = handleOne
  where
    handleOne = lift (recv r) >>= either (liftIO . invalidIPC) go

    go EndResponse = return ()
    go resp        = doResponse w resp >> handleOne

    invalidIPC l = do putStrLn $ "Got invalid resp: " ++ l
                      -- killPlugin p
                      signalProcess softwareTermination pid -- TEMP

doResponse w q = case q of
    ClientMsg msg  -> relayMsg msg
    ConfigLookup k -> do v <- lift2 $ getConfig k
                         reply $ ReqConfig k v
    UsersQuery ch  -> lift2 (getChan ch) >>= reply . ReqUsers ch
                                                   . fmap chanUsers
    ChansQuery     -> lift2 getChans >>= reply . ReqChans
    UserLookup u   -> lift (lookupUser u) >>= reply . ReqUser u
  where
    lift2 = lift . lift
    reply = liftIO . send w

 -- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
