{-# LANGUAGE DeriveDataTypeable #-}
module Buster.Plugin (InChan,
       chanMsg, commandPlugin, dispatchPlugins, makePlugin, processorPlugin,
       withPlugin) where

import Buster.IRC
import Buster.Misc
import Control.Monad.Reader
import Data.Dynamic
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable
import System.IO
import System.Plugins

data Plugin = Plugin {
    pluginCommands :: Map String (String -> InChan ()),
    pluginProcessor :: Maybe MessageProcessor
} deriving Typeable

type InChan = ReaderT Channel Net

commandPlugin cmds = Plugin (Map.fromList cmds) Nothing
processorPlugin p  = Plugin (Map.empty) (Just p)

dispatchPlugins :: [(String, Plugin)] -> MessageProcessor
dispatchPlugins ps msg = do
    mapM_ ($ msg) processors
    case snd msg of
      Chanmsg ch (ChatMsg t@('!':_)) -> eval pluginMap t `runReaderT` ch
      _                              -> return ()
  where
    processors = [pp | (_, Plugin { pluginProcessor = Just pp }) <- ps]
    pluginMap  = Map.fromList ps

eval ps ('!':s) = case words s of
    (w:_) -> do case Map.fold (collectCommands w) [] ps of
                  []  -> chanMsg ("No such command " ++ w ++ ".")
                  [c] -> c (stripLeft $ drop (length w) s)
                  cs  -> chanMsg ("Ambiguous command " ++ w ++ ".") -- TODO
    []    -> chanMsg "Command expected."
  where
    collectCommands w p cs = maybe cs (:cs) (w `Map.lookup` pluginCommands p)
eval _ _ = return ()

withPlugin :: String -> String -> ((String, Plugin) -> IO ()) -> IO ()
withPlugin lib name cont = do
    loaded <- loadDynamic (lib, "Buster.Machine." ++ name, "plugin")
    maybe (hPutStrLn stderr $ "Couldn't load " ++ name ++ " from " ++ lib)
          (>>= (cont . (,) name)) (loaded >>= fromDynamic)

makePlugin = toDyn :: IO Plugin -> Dynamic

chanMsg s = do nm <- ask
               lift $ write "PRIVMSG" [nm, s]

 -- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
