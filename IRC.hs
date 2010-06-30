{-# LANGUAGE RecordWildCards, ViewPatterns #-}
module IRC (Chan(..), IrcConfig(..), Plugin(..), chanMsg, runBot, write) where

import Data.Char
import Data.List
import Network
import System.IO
import Control.Exception
import Control.Monad.State
import Text.Printf
import Prelude hiding (catch, log)
import Misc
 
data IrcConfig = IrcConfig {
  ircServer, ircChannel, ircNick, ircUser, ircFullName :: String,
  ircPort :: Int
}

type Net = StateT Bot IO
data Bot = Bot { socket :: Handle,
                 plugins :: [Plugin] }

type Chan = StateT Channel Net
data Channel = Channel { chanName :: String }

data Plugin = Plugin {
    pluginName :: String,
    pluginCommands :: [(String, String -> Chan ())]
}

io = liftIO :: IO a -> Net a

runBot :: IrcConfig -> [Plugin] -> IO ()
runBot (IrcConfig {..}) ps = bracket setup disconnect loop
  where
    loop st = evalStateT go st `catch` (hPrint stderr :: IOException -> IO ())
    setup = do h <- connect
               return $ Bot h ps

    connect = notify $ do
        h <- connectTo ircServer (PortNumber (fromIntegral ircPort))
        dontBuffer h
        return h
    notify = bracket_ (printf "Connecting to %s ... " ircServer)
                      (putStrLn "done.")
    disconnect = hClose . socket
 
    go = do write "NICK" ircNick
            write "USER" (ircUser ++ " 0 * :" ++ ircFullName)
            write "JOIN" ircChannel
            gets socket >>= listen
 
listen :: Handle -> Net ()
listen h = forever $ do
    (src, s) <- (stripSource . init) `fmap` io (hGetLine h)
    case parseParams s of
      (msg:params, colon) -> handleMessage src msg params colon
      ([],         _)     -> return ()
  where
    stripSource (':':s) = let (src, s') = break (== ' ') s
                          in (Just src, stripLeft s')
    stripSource s       = (Nothing, stripLeft s)
    forever a = a >> forever a

    parseParams :: String -> ([String], Maybe String)
    parseParams = go []
      where
        go ws s = case stripLeft s of
                    ':':c -> (reverse ws, Just c)
                    s     -> let (word, rest) = span (/= ' ') s
                             in if null word then (reverse ws, Nothing)
                                         else go (word:ws) rest

warn = hPutStrLn stderr
log = putStrLn

write s t = do h <- gets socket
               io $ hPrintf h "%s %s\r\n" s t

handleMessage src msg ps cp' = case msg of
    "PING"    -> write "PONG" (':':cp)
    "NOTICE"  -> return ()
    "PRIVMSG" -> onPrivMsg (head ps) (extractAction cp)
    "JOIN"    -> io $ log $ "*** " ++ who ++ " joined " ++ cp
    "MODE"    -> unless (null ps || head ps == who) $
                   io $ log $ "*** " ++ who ++ " sets mode: " ++ ps' ++ " :"
                                     ++ cp
    _         -> unless (threeDigit msg) $
                   io $ warn ("Unknown message: " ++ msg ++ " " ++ ps')
  where
    nm  = maybe NoName parseName src
    who = pretty nm ""
    cp  = maybe "" id cp'
    ps' = unwords ps

    extractAction (stripPrefix "\001ACTION " -> Just act)
                  | not (null act) && last act == '\001' = Action nm (init act)
    extractAction msg = Message nm msg

    threeDigit t = length t == 3 && all isDigit t

data Message = Message Name String | Action Name String

instance Pretty Message where
  pretty (Message nm t) = ('<':) . pretty nm . ("> " ++) . showString t
  pretty (Action  nm t) = ("* " ++) . pretty nm . (' ':) . showString t

data Name = Name { nickName, userName, fullName :: String } | NoName

instance Pretty Name where
  pretty (Name n _ _) = showString n
  pretty NoName       = showString "<none>"

parseName s = let (nick, rest)  = break (== '!') s
                  (user, rest') = break (== '@') (tail rest)
              in Name nick user (tail rest')

onPrivMsg dest msg = do
    io $ putStrLn (pretty msg "")
    case msg of Message _ t@('!':_) -> evalStateT (eval t) (Channel dest)
                _                   -> return ()

eval :: String -> Chan ()
eval ('!':s) = case words s of
    (w:_) -> do ps <- lift $ gets plugins
                case foldl (collectCommands w) [] ps of
                  []  -> chanMsg ("No such command " ++ w ++ ".")
                  [c] -> c (stripLeft $ drop (length w) s)
                  cs  -> chanMsg ("Ambiguous command " ++ w ++ ".") -- TODO
    []    -> chanMsg "Command expected."
  where
    collectCommands w cs p = maybe cs (:cs) (w `lookup` pluginCommands p)
eval _ = return ()

chanMsg s = do nm <- gets chanName
               lift $ write "PRIVMSG" (nm ++ " :" ++ s)
 
-- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
