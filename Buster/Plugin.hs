{-# LANGUAGE DeriveDataTypeable, RecordWildCards, TypeSynonymInstances,
             ViewPatterns #-}
module Buster.Plugin (Cmd, Plugin,
       commandPlugin, hybridPlugin, pluginMain, processorPlugin,
       invoker, lookupConfig, medium, queryChans, queryUsers,
       respond, sendChat
       ) where

import Buster.Internal
import Buster.IRC
import Buster.Message
import Buster.Misc
import Control.Concurrent
import Control.Exception
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Typeable
import Prelude hiding (catch)
import System.IO
import System.Posix

type Plugin = ReaderT PluginState IO
type Cmd = ReaderT Invocation Plugin

data PluginImpl = PluginImpl {
    pluginCommands :: Map IString (String -> Cmd ()),
    pluginProcessor :: Maybe (ServerMsg -> Plugin (), Bool)
}

data PluginState = PluginState {
    pluginRead, pluginWrite :: Handle
}

data Invocation = Invocation User Target

invoker = (\(Invocation u _) -> u) `fmap` ask :: Cmd User
medium  = (\(Invocation _ m) -> m) `fmap` ask :: Cmd Target

data ReqException = ReqParseException String | ReqUnsupportedException
                    deriving (Show, Typeable)
instance Exception ReqException

commandPlugin cmds      = PluginImpl (cmdMap cmds) Nothing
processorPlugin p own   = PluginImpl (Map.empty) (Just (p, own))
hybridPlugin cmds p own = PluginImpl (cmdMap cmds) (Just (p, own))

cmdMap cs = Map.fromList [(IString k, v) | (k, v) <- cs]

pluginMain :: PluginImpl -> IO ()
pluginMain (PluginImpl {..}) = do
    installHandler openEndedPipe Default Nothing
    forkIO orphanChecker
    [r, w] <- mapM parsePipe ["READ", "WRITE"]
    mapM_ dontBuffer [stdout, stderr, r, w]
    send w api
    catch (loop `runReaderT` PluginState r w) handleError
    hClose r; hClose w
  where

    api = let (doesProc, selfProc) = maybe (False, False) (\b -> (True, snd b))
                                           pluginProcessor
          in API (Map.keysSet pluginCommands) doesProc selfProc
    loop = recv' >>= either invalidReq ((>> loop) . doReq)

    doReq (ReqProcess msg)             = check (($ msg) . fst) pluginProcessor
    doReq (ReqCommand cmd src ch args) =
        check (\f -> f args `runReaderT` Invocation src ch)
              (cmd `Map.lookup` pluginCommands)

    check f = maybe (liftIO $ throwIO ReqUnsupportedException)
                    ((>> send' EndResponse) . f)

    parsePipe = (>>= fdToHandle) . fmap (Fd . read . fromJust) . getEnv

    handleError :: ReqException -> IO ()
    handleError = print -- and leave loop

    orphanChecker = do threadDelay 5000000
                       id <- getParentProcessID
                       if id == 1 then raiseSignal softwareTermination
                                  else orphanChecker

respond :: String -> Cmd ()
respond s = medium >>= lift . flip sendChat s

sendChat :: Target -> String -> Plugin ()
sendChat t s = send' $ ClientMsg $ PrivMsg t (Chat s)

lookupConfig :: String -> Plugin (Maybe String)
lookupConfig s = do send' $ ConfigLookup s
                    recv' >>= either invalidReq gotIt
  where
    gotIt (ReqConfig k (Just v)) | k == s = return (Just v)
    gotIt _                               = return Nothing

queryChans :: Plugin (Map Channel ChannelState)
queryChans = do send' ChansQuery
                recv' >>= either invalidReq (\(ReqChans c) -> return c)

queryUsers :: Channel -> Plugin (Maybe (Map User Priv))
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

instance Context Cmd where
  contextLookup = lift . contextLookup

send' :: PluginResp -> Plugin ()
send' = (asks pluginWrite >>=) . flip send

recv' :: Plugin (Either String MachineReq)
recv' = asks pluginRead >>= recv

invalidReq = liftIO . throwIO . ReqParseException

 -- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
