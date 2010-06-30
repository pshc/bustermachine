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
 
    go = do write "NICK" [ircNick]
            write "USER" [ircUser, "0", "*", ircFullName]
            write "JOIN" [ircChannel]
            gets socket >>= listen
 
listen :: Handle -> Net ()
listen h = forever $ do
    (src, s) <- (stripSource . init) `fmap` io (hGetLine h)
    case parseParams s of msg:params -> handleMessage src (msg, params)
                          []         -> return ()
  where
    stripSource (':':s) = let (src, s') = break (== ' ') s
                          in (Just src, stripLeft s')
    stripSource s       = (Nothing, stripLeft s)
    forever a = a >> forever a
    parseParams = go []
      where
        go ws s = case stripLeft s of
                    ':':c -> reverse (c:ws)
                    s     -> let (word, rest) = span (/= ' ') s
                             in if null word then reverse ws
                                             else go (word:ws) rest

warn = hPutStrLn stderr
log = putStrLn

write :: String -> [String] -> Net ()
write msg ps = do h <- gets socket
                  ps' <- io $ joinParams ps
                  io $ hPrintf h "%s %s\r\n" msg ps'

joinParams [] = return ""
joinParams ps = go [] ps
  where
    go ps [c] = do let c' = case c of ':':_                -> ':':c
                                      _     | ' ' `elem` c -> ':':c
                                            | otherwise    -> c
                   return $ unwords $ reverse (c':ps)
    go ps (c:cs) = case c of
      ""                   -> omit "Omitted empty param"
      ':':p                -> omit $ "Omitted early colon param " ++ p
      _     | ' ' `elem` c -> omit $ "Space in non-end param '" ++ c ++ "'"
            | otherwise    -> go (c:ps) cs
      where
        omit = (>> go ps cs) . warn

handleMessage src msg = case msg of
    ("JOIN", [ch])   -> meta $ " joined " ++ ch
    ("KICK", ch:n:m) -> meta $ " kicked " ++ n ++ " from " ++ ch ++ exitMsg m
    ("MODE", at:m)   -> unless (at == who) $ meta $ " sets mode: " ++ unwords m
    ("NICK", new:_)  -> meta $ " changed nick to " ++ new
    ("PART", ch:ms)  -> meta $ " left " ++ ch ++ exitMsg ms
    ("NOTICE", m)    -> return ()
    ("PING", ps)     -> write "PONG" ps
    ("PRIVMSG", d:m) -> onPrivMsg d (extractAction (lastStr m))
    ("QUIT", m)      -> meta $ " quit " ++ exitMsg m
    (t, ps)          -> unless (threeDigit t) $ io $
                        joinParams (t:ps) >>= warn . ("Unknown message: " ++)
  where
    nm  = maybe NoName parseName src
    who = pretty nm ""
    meta = io . log . (("*** " ++ who) ++)
    exitMsg ss = if null ss then "" else " (" ++ last ss ++ ")"
    lastStr ss = if null ss then "" else last ss

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
               lift $ write "PRIVMSG" [nm, s]
 
-- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
