{-# LANGUAGE RecordWildCards, ViewPatterns #-}
module Buster.IRC (Config, MessageProcessor, Net,
                   Bot(..), ChannelState(..),
                   getConfig, requiredConfig, runBot, write) where

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
 
type Net = StateT Bot IO
data Bot = Bot { socket :: Handle,
                 botConfig :: Config,
                 processor :: MessageProcessor,
                 channels :: Map Chan ChannelState }

type MessageProcessor = (Name, ServerMsg) -> Net ()
data ChannelState = ChannelState { chanTopic :: Maybe String,
                                   chanNames :: Map Nick Priv }

type Config = String -> Maybe String

getConfig :: String -> Net (Maybe String)
getConfig = (`fmap` gets botConfig) . flip ($)

requiredConfig :: String -> Net String
requiredConfig s = do val <- ($ s) `fmap` gets botConfig
                      maybe (error $ "Config value '" ++ s ++ "' required")
                            return val

initialChan = ChannelState Nothing Map.empty
alterChan f ch = do bot <- get
                    put $ bot { channels = Map.alter f ch (channels bot) }

io = liftIO :: IO a -> Net a

runBot :: Config -> MessageProcessor -> IO ()
runBot cfg mp = bracket setup disconnect loop
  where
    loop st = evalStateT go st `catch` (hPrint stderr :: IOException -> IO ())
    setup = do h <- connect
               return $ Bot h cfg mp Map.empty
    config s = maybe (error $ "Need config value for " ++ s) id (cfg s)
    server   = config "server"
    connect = notify $ do
        let port = PortNumber $ fromIntegral $ maybe 6667 read $ cfg "port"
        h <- connectTo server port
        dontBuffer h
        return h
    notify = bracket_ (printf "Connecting to %s ... " server)
                      (putStrLn "done.")
    disconnect = hClose . socket
 
    go = do write "NICK" [config "nick"]
            write "USER" [maybe (config "nick") id $ cfg "user",
                          "0", "*", maybe "" id $ cfg "fullName"]
            write "JOIN" [config "channel"]
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

parseMessage msg = case msg of
    ("AWAY", [])       -> return [Away Nothing]
    ("AWAY", [m])      -> return [Away (Just m)]
    ("INVITE", [ch])   -> return [Invite (parseChan ch)]
    ("JOIN", [ch])     -> return [Join (parseChan ch)]
    ("KICK", [ch,n])   -> return [Kick (parseChan ch) n Nothing]
    ("KICK", [ch,n,m]) -> return [Kick (parseChan ch) n (Just m)]
    ("MODE", ch:ms)    -> return $ if isChan ch then [Mode (parseChan ch) ms]
                                                else []
    ("NICK", [n])      -> return [NickChange n]
    ("PART", [ch])     -> return [Part (parseChan ch) Nothing]
    ("PART", [ch,m])   -> return [Part (parseChan ch) (Just m)]
    ("NOTICE", [ch,m]) -> return [Notice (parseTarget ch) m]
    ("PING", ps)       -> write "PONG" ps >> return []
    ("PRIVMSG", [d,m]) -> return [PrivMsg (parseTarget d) (extractAction m)]
    ("TOPIC", [ch,t])  -> return [Topic (parseChan ch) t]
    ("QUIT", [m])      -> return [Quit m]
    (t, ps) | threeDigit t -> numCode (read t, ps) >> return []
            | otherwise    -> io $ do ps' <- joinParams (t:ps)
                                      warn $ "Unknown message: " ++ ps'
                                      return []
  where
    extractAction (stripPrefix "\001ACTION " -> Just act)
                  | not (null act) && last act == '\001' = Action (init act)
    extractAction msg = Chat msg

    threeDigit t = length t == 3 && all isDigit t

parseChan ('#':ch) = (:#) ch
parseChan ('&':ch) = (:&) ch
parseChan ('+':ch) = (:+) ch
parseChan ch       = (:#) ch

parseTarget t | isChan t  = Chan (parseChan t)
              | otherwise = Nick t

numCode msg = case msg of
    (331, ch:_)   -> setTopic Nothing `alterChan` parseChan ch
    (332, [ch,t]) -> setTopic (Just t) `alterChan` parseChan ch
    (353, nms)    -> case dropWhile (not . isChan) nms of
                       ch:nms' -> alterChan (setNames (parseChanNicks nms'))
                                            (parseChan ch)
                       _ -> io $ warn $ "Bad NAMES message: " ++ unwords nms
    _             -> return ()
  where
    setTopic t = Just . maybe (initialChan { chanTopic = t })
                              (\c -> c { chanTopic = t })
    setNames nms = Just . maybe (initialChan { chanNames = nms })
                                (\c -> c { chanNames = nms })
    parseChanNicks = Map.fromList . map parseNick
    parseNick ('@':nm) = (nm, Op)
    parseNick ('+':nm) = (nm, Voice)
    parseNick nm       = (nm, Regular)

-- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
