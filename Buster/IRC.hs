{-# LANGUAGE RecordWildCards, ViewPatterns #-}
module Buster.IRC (Config, MessageProcessor, Net,
                   Bot(..), ChannelState(..),
                   getChan, getChans, alterChan,
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
                 users :: Map User UserInfo,
                 userNicks :: Map String User,
                 idCtr :: Int,
                 channels :: Map Chan ChannelState }

initBot cfg mp h = Bot h cfg mp Map.empty Map.empty 1 Map.empty

type MessageProcessor = ServerMsg -> Net ()
data ChannelState = ChannelState { chanTopic :: Maybe String,
                                   chanUsers :: Map User Priv }
                    deriving (Read, Show)

type Config = String -> Maybe String

getConfig :: String -> Net (Maybe String)
getConfig = (`fmap` gets botConfig) . flip ($)

requiredConfig :: String -> Net String
requiredConfig s = do val <- ($ s) `fmap` gets botConfig
                      maybe (error $ "Config value '" ++ s ++ "' required")
                            return val

initialChan = ChannelState Nothing Map.empty
getChan ch = Map.lookup ch `fmap` gets channels
getChans :: Net (Map Chan ChannelState)
getChans   = gets channels
alterChan f ch = do bot <- get
                    put $ bot { channels = Map.alter f ch (channels bot) }

io = liftIO :: IO a -> Net a

runBot :: Config -> MessageProcessor -> IO ()
runBot cfg mp = bracket (initBot cfg mp `fmap` connect) disconnect loop
  where
    loop st = evalStateT go st `catch` (hPrint stderr :: IOException -> IO ())
    config s = maybe (error $ "Need config value for " ++ s) id (cfg s)
    server = config "server"
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
    case parseParams [] s of
      "PING":ps -> write "PONG" ps
      ms -> parseMessage ms >>= (\ms' -> unless (null ms') $
                                         maybe (warnIgnored ms') (go ms') src)
  where
    stripSource (':':s) = let (src, s') = break (== ' ') s
                          in (Just src, stripLeft s')
    stripSource s       = (Nothing, stripLeft s)

    parseParams ws (stripLeft -> ':':c) = reverse (c:ws)
    parseParams ws s = let (word, rest) = span (/= ' ') (stripLeft s)
                       in if null word then reverse ws
                                       else parseParams (word:ws) rest

    go ms nm = do user <- userFromName nm
                  let msgs = zip (repeat user) ms
                  mapM_ updateState msgs
                  gets processor >>= forM_ msgs

    updateState (u, msg) = case msg of
        NickChange n -> lookupUser u >>= updateNick n
        _            -> return ()
      where
        updateNick nick Nothing     = userFromName nick >> return ()
        updateNick nick (Just info) = do
            let oldNick = userNick info
            updateUser u (info { userNick = nick })
            bot <- get
            put $ bot { userNicks = Map.insert nick u
                                    (oldNick `Map.delete` userNicks bot) }

    warnIgnored = io . warn . ("Message(s) ignored due to no source: " ++)
                            . intercalate ", " . map show

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

lookupUser :: User -> Net (Maybe UserInfo)
lookupUser = (`fmap` gets users) . Map.lookup

updateUser :: User -> UserInfo -> Net ()
updateUser u i = do bot <- get
                    put $ bot { users = Map.insert u i (users bot) }

userFromName nm = do let (nick, rest)  = break (== '!') nm
                         (user, rest') = break (== '@') (tail' rest)
                     Map.lookup nick `fmap` gets userNicks
                         >>= maybe (newUser nick user (tail' rest')) return
  where
    tail' s = if null s then "" else tail s
    newUser nick user host = do
        bot@(Bot {..}) <- get
        let uid = UserID idCtr
        put $ bot { users = Map.insert uid (UserInfo nick user host) users,
                    userNicks = Map.insert nick uid userNicks,
                    idCtr = idCtr + 1 }
        return uid

parseMessage msg = case msg of
    ["AWAY"]            -> return [Away Nothing]
    ["AWAY", m]         -> return [Away (Just m)]
    ["ERROR", e]        -> do io $ warn $ "Server error report: " ++ e
                              return []
    ["INVITE", ch]      -> return [Invite (parseChan ch)]
    ["JOIN", ch]        -> return [Join (parseChan ch)]
    ["KICK", ch,n]      -> do u <- userFromName n
                              return [Kick (parseChan ch) u Nothing]
    ["KICK", ch,n,m]    -> do u <- userFromName n
                              return [Kick (parseChan ch) u (Just m)]
    "MODE":ch:ms        -> return $ if isChan ch then [Mode (parseChan ch) ms]
                                                 else []
    ["NICK", n]         -> return [NickChange n]
    ["PART", ch]        -> return [Part (parseChan ch) Nothing]
    ["PART", ch,m]      -> return [Part (parseChan ch) (Just m)]
    ["NOTICE", ch,m]    -> do t <- parseTarget ch
                              return [Notice t m]
    ["PRIVMSG", d,m]    -> do t <- parseTarget d
                              return [PrivMsg t (extractAction m)]
    ["TOPIC", ch,t]     -> return [Topic (parseChan ch) t]
    ["QUIT", m]         -> return [Quit m]
    t:ps | threeDigit t -> numCode (read t, ps) >> return []
         | otherwise    -> io $ do ps' <- joinParams (t:ps)
                                   warn $ "Unknown message: " ++ ps'
                                   return []
    []                  -> return []
  where
    extractAction (stripPrefix "\001ACTION " -> Just act)
                  | not (null act) && last act == '\001' = Action (init act)
    extractAction msg = Chat msg

    threeDigit t = length t == 3 && all isDigit t

parseChan ('#':ch) = (:#) ch
parseChan ('&':ch) = (:&) ch
parseChan ('+':ch) = (:+) ch
parseChan ch       = (:#) ch

parseTarget t | isChan t  = return $ Chan (parseChan t)
              | otherwise = User `fmap` userFromName t

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
    setNames _ _ = Nothing -- TEMP
    --setNames nms = Just . maybe (initialChan { chanUsers = nms })
    --                            (\c -> c { chanUsers = nms })
    parseChanNicks = Map.fromList . map parseNick
    parseNick ('@':nm) = (nm, Op)
    parseNick ('+':nm) = (nm, Voice)
    parseNick nm       = (nm, Regular)

-- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
