{-# LANGUAGE RecordWildCards, ViewPatterns #-}
module Buster.Machine (setupMachine) where

import Buster.Internal
import Buster.IRC
import Buster.Message
import Buster.Misc
import Control.Exception
import Control.Monad.Reader
import Data.IORef
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO
import System.Posix
import System.Timeout

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

type RelayBack = ReaderT Target Users

relayBack :: String -> RelayBack ()
relayBack msg = ask >>= (lift . flip privMsg msg)

dispatchPlugins :: IORef Machine -> MessageProcessor
dispatchPlugins mRef (src, msg) = do
    Machine { machPlugins = ps } <- liftIO $ readIORef mRef
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
      | c `elem` builtinCmds = builtinCmd ps mRef c ws
    doCmd ps t s (c:_) = case Map.fold (collectPlugins $ IString c) [] ps of
        []  -> relayBack ("No such command " ++ c ++ ".")
        [p] -> lift $ do let args = stripLeft $ drop (length c) s
                         send (ipcWrite p) (ReqCommand (IString c) src t args)
                         handleResponses p
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
builtinCmd ps r c [nm] = case (c, nm `Map.lookup` ps) of
    ("list", Just p)   -> relayBack $ ipcCmdList p
    ("load", Nothing)  -> loadIt
    ("load", Just _)   -> relayBack (nm ++ " is already loaded.")
    ("reload", Just _) -> liftIO (unloadPlugin r nm) >> loadIt
    ("unload", Just _) -> liftIO (unloadPlugin r nm) >> gotcha
    _                  -> relayBack $ "No such plugin '" ++ nm ++ ".'"
  where
    loadIt = do r <- liftIO $ loadPlugin r nm
                if r then gotcha else relayBack "Plugin load failed."
    gotcha = relayBack "Gotcha."
    ipcCmdList = showCmdList . extract . Set.toList . apiCommandSet . ipcAPI
    extract = map (\(IString s) -> s)

builtinCmd _ _ _ _ = relayBack "Plugin expected."

showCmdList []   = "That plugin has no commands."
showCmdList cmds = (intercalate ", " cmds)

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
