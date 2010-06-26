{-# LANGUAGE RecordWildCards, ViewPatterns #-}

import Data.Char
import Data.List
import Network
import System.IO
import System.Exit
import Control.Exception
import Control.Monad.State
import Text.Printf
import Prelude hiding (catch, log)
import Plugin
import Misc
 
server   = "irc.opera.com"
port     = 6667
chan     = "#cantide"
nick     = "canti"
user     = "nono"
full     = "Buster Machine No. 7"

type Net = StateT Bot IO
data Bot = Bot { socket :: Handle,
                 plugins :: [Plugin] }

main :: IO ()
main = do
    dontBuffer stdout; dontBuffer stderr
    bracket setup disconnect loop
  where
    loop st = evalStateT run st `catch` (hPrint stderr :: IOException -> IO ())
    setup = do ps <- resource
               h <- connect
               return $ Bot h ps

    connect = notify $ do
        h <- connectTo server (PortNumber (fromIntegral port))
        dontBuffer h
        return h
    notify = bracket_ (printf "Connecting to %s ... " server)
                      (putStrLn "done.")
    disconnect = hClose . socket
 
    dontBuffer = flip hSetBuffering NoBuffering

-- Join a channel, and start processing commands
run :: Net ()
run = do
    write "NICK" nick
    write "USER" (user ++ " 0 * :" ++ full)
    write "JOIN" chan
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

handleMessage src msg ps cp' = case msg of
    "PING"    -> write "PONG" (':':cp)
    "NOTICE"  -> return ()
    "PRIVMSG" -> onPrivMsg (head ps) (extractAction cp)
    "JOIN"    -> log $ "*** " ++ who ++ " joined " ++ cp
    "MODE"    -> unless (null ps || head ps == who) $
                   log $ "*** " ++ who ++ " sets mode: " ++ ps' ++ " :" ++ cp
    _         -> unless (threeDigit msg) $
                   warn ("Unknown message: " ++ msg ++ " " ++ ps')
  where
    nm  = maybe NoName parseName src
    who = pretty nm ""
    cp  = maybe "" id cp'
    ps' = unwords ps

    extractAction (stripPrefix "\001ACTION " -> Just act)
                  | not (null act) && last act == '\001' = Action nm (init act)
    extractAction msg = Message nm msg

    threeDigit t = length t == 3 && all isDigit t

log = io . putStrLn

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

warn = io . putStrLn

onPrivMsg dest msg = do
    io $ putStrLn (pretty msg "")
    case msg of Message _ t@('!':_) -> eval t
                _                   -> return ()

eval "!quit" = do write "QUIT" ":Exiting"
                  io (exitWith ExitSuccess)
eval (stripPrefix "!id " -> Just x) = privmsg x
eval ('!':s) = case words s of
    (w:_) -> do ps <- gets plugins
                case foldl (\cs p -> maybe cs (:cs)
                                     (w `lookup` pluginCommands p)) [] ps of
                  []  -> privmsg ("No such command " ++ w ++ ".")
                  [c] -> do let ss = stripLeft $ drop (length w) s
                            io (c ss) >>= mapM_ respond
                  cs  -> privmsg ("Ambiguous command.") -- TODO
    []    -> privmsg "Command expected."
eval _ = return ()

respond (ChannelMessage t) = privmsg t

privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)
 
write s t = do h <- gets socket
               io $ hPrintf h "%s %s\r\n" s t

io = liftIO :: IO a -> Net a
 
-- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
