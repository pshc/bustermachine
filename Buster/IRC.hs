{-# LANGUAGE RecordWildCards, TupleSections, TypeSynonymInstances,
             ViewPatterns #-}
module Buster.IRC (Config, MessageProcessor, Net, Users,
                   Bot(..), ChannelState(..),
                   getChan, getChans, alterChan, lookupUser,
                   getConfig, requiredConfig, runBot, write) where

import Buster.Message
import Buster.Misc
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad.Reader
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
                 channels :: Map Channel ChannelState }

type Users = ReaderT (MVar UsersState) Net

initUsers nick = let (self, info) = (UserID 0, UserInfo nick "" "")
                 in UsersState { selfUser = self,
                                 users = Map.singleton self info,
                                 userNicks = Map.singleton nick self,
                                 idCtr = 1 }

instance Context Users where
  contextLookup k = Map.lookup k `fmap` asks' users

ask' :: Users UsersState
ask' = ask >>= io . readMVar

asks' :: (UsersState -> a) -> Users a
asks' = (`fmap` ask')

modify' :: (UsersState -> UsersState) -> Users ()
modify' f = ask >>= io . flip modifyMVar_ (return . f)

type MessageProcessor = Chan IRCMsg -> ServerMsg -> Users ()
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
getChans :: Net (Map Channel ChannelState)
getChans = gets channels
alterChan :: (Maybe ChannelState -> Maybe ChannelState) -> Channel -> Net ()
alterChan f ch = modify (\b -> b { channels = Map.alter f ch (channels b) })

io = liftIO :: IO a -> Users a

runBot :: Config -> MessageProcessor -> IO ()
runBot cfg mp = bracket connect hClose ready
  where
    config s = maybe (error $ "Need config value for " ++ s) id (cfg s)
    (nick, server) = (config "nick", config "server")

    connect = notify $ do
        let port = PortNumber $ fromIntegral $ maybe 6667 read $ cfg "port"
        h <- connectTo server port
        dontBuffer h
        hSetBinaryMode h True
        return h
    notify = bracket_ (printf "Connecting to %s ... " server)
                      (putStrLn "done.")
    ready h = do let bot = Bot { socket = h, botConfig = cfg, processor = mp,
                                 channels = Map.empty }
                 evalStateT go bot `catch` onErr
    go = do write "NICK" [nick]
            write "USER" [maybe nick id $ cfg "user",
                          "0", "*", maybe "" id $ cfg "fullName"]
            write "JOIN" [config "channel"]
            h <- gets socket
            chan <- liftIO newChan
            liftIO $ forkIO $ writeThread h chan
            users <- liftIO $ newMVar (initUsers nick)
            listen h chan `runReaderT` users

    onErr :: IOException -> IO ()
    onErr = hPrint stderr
 
listen :: Handle -> Chan IRCMsg -> Users ()
listen h chan = forever $ do
    (src, s) <- (stripSource . init) `fmap` io (hGetLine h)
    case parseParams [] s of
      "PING":ps -> lift $ write "PONG" ps
      ps        -> do ms <- parseMessage ps
                      unless (null ms) $ maybe (warnIgnored ms)
                                               (runProcessor ms) src
  where
    stripSource (':':s) = let (src, s') = break (== ' ') s
                          in (Just src, stripLeft s')
    stripSource s       = (Nothing, stripLeft s)

    parseParams ws (stripLeft -> ':':c) = reverse (c:ws)
    parseParams ws s = let (word, rest) = span (/= ' ') (stripLeft s)
                       in if null word then reverse ws
                                       else parseParams (word:ws) rest

    runProcessor ms nm = do user <- userFromName nm
                            let msgs = zip (repeat user) ms
                            mapM_ updateState msgs
                            proc <- lift (gets processor)
                            mapM_ (proc chan) msgs

    updateState :: (User, IRCMsg) -> Users ()
    updateState (u, msg) = case msg of
        Join ch      -> lift $ join `alterChan` ch
        Kick ch u' _ -> u' `left` ch
        NickChange n -> lookupUser u >>= updateNick n
        Part ch _    -> u `left` ch
        Quit _       -> do chs <- Map.keys `fmap` lift (gets channels)
                           mapM_ (u `left`) chs
        _            -> return ()
      where
        modUsers f c = c { chanUsers = f (chanUsers c) }
        join = Just . maybe (initialChan {chanUsers = Map.singleton u Regular})
                            (modUsers $ Map.insert u Regular)
        u `left` ch = do self <- asks' selfUser
                         lift $ maybe Nothing (leave self) `alterChan` ch
          where
            leave self | u == self = const Nothing
                       | otherwise = Just . modUsers (Map.delete u)
        updateNick nick Nothing     = userFromName nick >> return ()
        updateNick nick (Just info) = do
            let oldNick = userNick info
            updateUser u (info { userNick = nick })
            modify' (\bot -> bot { userNicks = Map.insert nick u
                                   (oldNick `Map.delete` userNicks bot) })

    warnIgnored = io . warn . ("Message(s) ignored due to no source: " ++)
                            . intercalate ", " . map show

warn = hPutStrLn stderr
log = putStrLn

write :: String -> [String] -> Net ()
write msg ps = do h <- gets socket
                  ps' <- liftIO $ joinParams ps
                  liftIO $ hPrintf h "%s %s\r\n" msg ps'

writeThread :: Handle -> Chan IRCMsg -> IO ()
writeThread h chan = getChanContents chan >>= mapM_ writeMsg
  where
    writeMsg msg = putStrLn (show msg)

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

lookupUser :: User -> Users (Maybe UserInfo)
lookupUser = (`fmap` asks' users) . Map.lookup

updateUser :: User -> UserInfo -> Users ()
updateUser u i = modify' $ \bot -> bot { users = Map.insert u i (users bot) }

userFromName :: String -> Users User
userFromName nm = do let (nick, tail' -> rest) = break (== '!') nm
                         (u,    tail' -> mask) = break (== '@') rest
                         ui                    = UserInfo nick u mask
                     user <- Map.lookup nick `fmap` asks' userNicks
                     maybe (ask >>= io . (`modifyMVar` newUser nick ui))
                           return user
  where
    tail' s = if null s then "" else tail s
    newUser nick ui us@(UsersState {..}) = do
        let uid = UserID idCtr
            us' = us { users = Map.insert uid ui users,
                       userNicks = Map.insert nick uid userNicks,
                       idCtr = idCtr + 1 }
        return (us', uid)

parseMessage :: [String] -> Users [IRCMsg]
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
    t:n:ps | digits3 t  -> do self <- asks' selfUser
                              lookupUser self >>= checkOwnNick n self
                              numCode (read t, ps)
                              return []
    t:ps                -> io $ do ps' <- joinParams (t:ps)
                                   warn $ "Unknown message: " ++ ps'
                                   return []
    []                  -> return []
  where
    extractAction (stripPrefix "\001ACTION " -> Just act)
                  | not (null act) && last act == '\001' = Action (init act)
    extractAction msg = Chat msg

    checkOwnNick n self (Just info) | n /= userNick info = 
        updateUser self (info { userNick = n })
    checkOwnNick _ _ _ = return ()

    digits3 t = length t == 3 && all isDigit t

parseChan ('#':ch) = (:#) ch
parseChan ('&':ch) = (:&) ch
parseChan ('+':ch) = (:+) ch
parseChan ch       = (:#) ch

parseTarget t | isChan t  = return $ Channel (parseChan t)
              | otherwise = User `fmap` userFromName t

numCode :: (Int, [String]) -> Users ()
numCode msg = case msg of
    (331, ch:_)   -> lift $ setTopic Nothing `alterChan` parseChan ch
    (332, [ch,t]) -> lift $ setTopic (Just t) `alterChan` parseChan ch
    (333, ch:whom:when:_) -> return () -- Topic set when and by whom
    -- NAMES result. TODO: buffer until 366 received (End of /NAMES list)
    (353, dropWhile (not . isChan) -> ch:ns) -> do
        us <- forM ns $ \n -> let (nm, priv) = parseNick n
                              in (, priv) `fmap` userFromName nm
        lift $ setUsers (Map.fromList us) `alterChan` parseChan ch
    (421, [cmd,err])  -> unless ("Quit" `isInfixOf` err) $
                             io $ warn $ "Server: " ++ cmd ++ ": " ++ err
    (433, [nick,err]) -> lift $ write "NICK " [nick ++ "`"]
    _ -> return ()
  where
    setTopic t = Just . maybe (initialChan { chanTopic = t })
                              (\c -> c { chanTopic = t })
    setUsers us = Just . maybe (initialChan { chanUsers = us })
                               (\c -> c { chanUsers = us })
    parseNick ('@':nm) = (nm, Op)
    parseNick ('+':nm) = (nm, Voice)
    parseNick nm       = (nm, Regular)

-- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
