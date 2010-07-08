{-# LANGUAGE RecordWildCards, ViewPatterns #-}
module Buster.IRC (MessageProcessor, Net, Bot(..), IrcConfig(..),
                   runBot, write) where

import Buster.Message
import Buster.Misc
import Control.Exception
import Control.Monad.State
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Network
import System.IO
import Text.Printf
import Prelude hiding (catch, log)
 
data IrcConfig = IrcConfig {
  ircServer, ircChannel, ircNick, ircUser, ircFullName :: String,
  ircPort :: Int
}

type Net = StateT Bot IO
data Bot = Bot { socket :: Handle,
                 processor :: MessageProcessor,
                 channels :: Map Channel ChannelState }

type MessageProcessor = (Name, ServerMsg) -> Net ()
data ChannelState = ChannelState { chanTopic :: Maybe String }

initialChan = ChannelState Nothing
alterChan f ch = do bot <- get
                    put $ bot { channels = Map.alter f ch (channels bot) }

io = liftIO :: IO a -> Net a

runBot :: IrcConfig -> MessageProcessor -> IO ()
runBot (IrcConfig {..}) mp = bracket setup disconnect loop
  where
    loop st = evalStateT go st `catch` (hPrint stderr :: IOException -> IO ())
    setup = do h <- connect
               return $ Bot h mp Map.empty

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
                                     p <- gets processor
                                     ms <- parseMessage (m, ps)
                                     mapM_ (p . (,) nm) ms
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

parseName s = let (nick, rest)  = break (== '!') s
                  (user, rest') = break (== '@') (tail rest)
              in Name nick user (tail rest')

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
    (t, ps) | threeDigit t -> numCode (read t, ps) >> return []
            | otherwise    -> io $ do ps' <- joinParams (t:ps)
                                      warn $ "Unknown message: " ++ ps'
                                      return []
  where
    isChan (x:_) = (x == '#' || x == '&')
    isChan _     = False

    extractAction (stripPrefix "\001ACTION " -> Just act)
                  | not (null act) && last act == '\001' = Action (init act)
    extractAction msg = ChatMsg msg

    threeDigit t = length t == 3 && all isDigit t

numCode msg = case msg of
    (331, ch:_)   -> setTopic Nothing `alterChan` ch
    (332, [ch,t]) -> setTopic (Just t) `alterChan` ch
    _             -> return ()
  where
    setTopic t = Just . maybe (initialChan { chanTopic = t })
                              (\c -> c { chanTopic = t })

-- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
