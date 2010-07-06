{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, RecordWildCards,
             ViewPatterns #-}
module Buster.IRC (InChan(..), IrcConfig(..), Net(..), Plugin(..),
                   ServerMsg(..), chanMsg, makePlugin, runBot, withPlugin,
                   write) where

import Buster.Misc
import Control.Exception
import Control.Monad.State
import Data.Char
import Data.Dynamic
import Data.List
import Data.Typeable
import Network
import System.IO
import System.Plugins
import Text.Printf
import Prelude hiding (catch, log)
 
data IrcConfig = IrcConfig {
  ircServer, ircChannel, ircNick, ircUser, ircFullName :: String,
  ircPort :: Int
}

type Net = StateT Bot IO
data Bot = Bot { socket :: Handle,
                 plugins :: [Plugin] }

type Channel = String
type InChan = StateT ChannelState Net
data ChannelState = ChannelState { chanName :: Channel }

data Plugin = Plugin {
    pluginName :: String,
    pluginCommands :: [(String, String -> InChan ())],
    pluginProcessor :: Maybe ((Name, ServerMsg) -> Net ())
} deriving Typeable

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
    case parseParams s of m:ps -> do let nm = maybe NoName parseName src
                                     ms <- parseMessage (m, ps)
                                     plgs <- gets plugins
                                     forM_ ms $ \msg -> do
                                       mapM_ (runProcessor (nm, msg)) plgs
                                       dispatch (nm, msg)
                          []   -> return ()
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

    runProcessor msg (Plugin {..}) = maybe (return ()) ($ msg) pluginProcessor

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

type Nick = String
instance Pretty (Name, ServerMsg) where
  pretty (nm, msg) = case msg of
    Away m      -> who . maybe (s "is no longer away")
                               (\awayMsg -> s "is away" . paren awayMsg) m
    Invite ch   -> who . s "invited you to " . s ch
    Join ch     -> who . s "joined " . s ch
    Kick ch n m -> who . s "kicked " . s n . s " from " . s ch . maybeParen m
    Mode ch ms  -> who . foldl concatWords (s "sets mode:") ms
    Nick n      -> who . s "changed nick to " . s n
    Part ch m   -> who . s "left " . s ch . maybeParen m
    Notice n m  -> s "Notice from " . s n . s ": " . s m
    Privmsg m   -> s "(PM) " . pretty (nm, m)
    Chanmsg _ m -> pretty (nm, m)
    Topic ch m  -> who . s "changed " . s ch . s "'s topic to " . s m
    Quit m      -> s " quit" . paren m
   where
    s               = showString
    who             = s "*** " . pretty nm . s " "
    paren m         = s " (" . s m . s ")"
    maybeParen      = maybe id paren
    concatWords f m = f . (' ':) . s m

data ServerMsg = Away (Maybe String) | Invite Channel | Join Channel
                 | Kick Channel Nick (Maybe String) | Mode Channel [String]
                 | Nick Nick | Part Channel (Maybe String) | Notice Nick String
                 | Privmsg ChatMsg | Chanmsg Channel ChatMsg
                 | Topic Channel String | Quit String

parseMessage :: (String, [String]) -> Net [ServerMsg]
parseMessage msg = case msg of
    ("AWAY", [])       -> return [Away Nothing]
    ("AWAY", [m])      -> return [Away (Just m)]
    ("INVITE", [ch])   -> return [Invite ch]
    ("JOIN", [ch])     -> return [Join ch]
    ("KICK", [c,n])    -> return [Kick c n Nothing]
    ("KICK", [c,n,m])  -> return [Kick c n (Just m)]
    ("MODE", ch:ms)    -> return $ if isChan ch then [Mode ch ms] else []
    ("NICK", [n])      -> return [Nick n]
    ("PART", [ch])     -> return [Part ch Nothing]
    ("PART", [ch,m])   -> return [Part ch (Just m)]
    ("NOTICE", [n,t])  -> return [Notice n t]
    ("PING", ps)       -> write "PONG" ps >> return []
    ("PRIVMSG", [d,m]) -> do let a = extractAction m
                             return [if isChan d then Chanmsg d a
                                                 else Privmsg a]
    ("TOPIC", [ch,t])  -> return [Topic ch t]
    ("QUIT", [m])      -> return [Quit m]
    (t, ps)            -> do unless (threeDigit t) $ io $ do
                               ps' <- joinParams (t:ps)
                               warn $ "Unknown message: " ++ ps'
                             return []
  where
    isChan (x:_) = (x == '#' || x == '&')
    isChan _     = False

    extractAction (stripPrefix "\001ACTION " -> Just act)
                  | not (null act) && last act == '\001' = Action (init act)
    extractAction msg = ChatMsg msg

    threeDigit t = length t == 3 && all isDigit t

data ChatMsg = ChatMsg String | Action String

instance Pretty (Name, ChatMsg) where
  pretty (nm, ChatMsg t) = ('<':) . pretty nm . ("> " ++) . showString t
  pretty (nm, Action t)  = ("* " ++) . pretty nm . (' ':) . showString t

data Name = Name { nickName, userName, fullName :: String } | NoName

instance Pretty Name where
  pretty (Name n _ _) = showString n
  pretty NoName       = showString "<none>"

parseName s = let (nick, rest)  = break (== '!') s
                  (user, rest') = break (== '@') (tail rest)
              in Name nick user (tail rest')

dispatch :: (Name, ServerMsg) -> Net ()
dispatch msg = case snd msg of
    Chanmsg ch (ChatMsg t@('!':_)) -> eval t `evalStateT` ChannelState ch
    _                              -> return ()

eval :: String -> InChan ()
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

withPlugin :: String -> String -> (Plugin -> IO ()) -> IO ()
withPlugin lib mod cont = do
    loaded <- loadDynamic (lib, "Buster.Machine." ++ mod, "plugin")
    maybe (hPutStrLn stderr $ "Couldn't load " ++ mod ++ " from " ++ lib)
          (>>= cont) (loaded >>= fromDynamic)

makePlugin = toDyn :: IO Plugin -> Dynamic
 
-- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
