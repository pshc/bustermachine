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
                     return $ dispatchPlugins m
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
        Just (Right a) -> return $ Just (PluginIPC pid r w a)
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
              --- XXX: Could block indefinitely from a malicious plugin
              getProcessStatus True True (ipcPID p)
              return $ mach { machPlugins = ps' }

killPlugin = signalProcess softwareTermination . ipcPID

data PluginIPC = PluginIPC {
    ipcPID :: ProcessID,
    ipcRead, ipcWrite :: Handle,
    ipcAPI :: API
}

type RelayBack = ReaderT Target Users

relayBack :: String -> RelayBack ()
relayBack msg = ask >>= (lift . flip privMsg msg)

dispatchPlugins :: MVar Machine -> MessageProcessor
dispatchPlugins mv (src, msg) = do
    Machine { machPlugins = ps } <- liftIO $ readMVar mv
    mapM_ doProc [p | p <- Map.elems ps, apiHasProcessor (ipcAPI p)]
    case msg of
      PrivMsg t (Chat ('!':s)) -> runReaderT (doCmd ps t s (words s))
                                             (reflect t)
      _                        -> return ()
  where
    reflect ch@(Chan _) = ch
    reflect _           = User src

    doProc p = do send (ipcWrite p) (ReqProcess (src, msg))
                  handleResponses p

    doCmd ps _ _ ((lower -> c):ws)
      | c `elem` builtinCmds = builtinCmd ps mv c ws
    doCmd ps t s (c:_) = case Map.fold (collectPlugins $ IString c) [] ps of
        []  -> relayBack ("No such command " ++ c ++ ".")
        [p] -> do let args = stripLeft $ drop (length c) s
                  lift $ send (ipcWrite p) (ReqCommand (IString c) src t args)
                  forkResponseHandler p
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

forkResponseHandler :: PluginIPC -> RelayBack ()
forkResponseHandler p = do
    -- TEMP: I N C E P T I O N
    a <- lift (lift get)
    b <- lift get
    c <- get
    liftIO $ forkIO $ do r <- (5*1000000) `timeout` execStateT
                                                     (execStateT
                                                       (execStateT go
                                                        c)
                                                      b)
                                                    a
                         when (isNothing r) $ putStrLn "Command timed out."
    return ()
  where
    go = lift $ handleResponses p

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
                 lift $ write "PRIVMSG" [dest, s]

 -- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent: