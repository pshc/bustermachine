{-# LANGUAGE ViewPatterns, ScopedTypeVariables #-}

import Data.Char
import Data.List
import Network
import System.IO
import System.Time
import System.Exit
import Control.Monad.Reader
import Control.Exception
import Text.Printf
import Prelude hiding (catch, log)
import Plugin
 
server   = "irc.opera.com"
port     = 6667
chan     = "#cantide"
nick     = "canti"
user     = "nono"
full     = "Buster Machine No. 7"
 
-- The 'Net' monad, a wrapper over IO, carrying the bot's immutable state.
-- A socket and the bot's start time.
type Net = ReaderT Bot IO
data Bot = Bot { socket :: Handle, plugins :: API, starttime :: ClockTime }
 
main :: IO ()
main = do
    dontBuffer stdout
    bracket startup disconnect loop
  where
    startup = do let p = resource
                 t <- getClockTime
                 h <- connect
                 return $ Bot h p t
    loop st = runReaderT run st --`catch` \(e :: Exception) -> return ()

    connect = notify $ do
        h <- connectTo server (PortNumber (fromIntegral port))
        dontBuffer h
        return h
      where
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
    asks socket >>= listen
 
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

    stripLeft = dropWhile (== ' ')

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
    "PRIVMSG" -> maybe (warn ("No-source PRIVMSG: " ++ cp))
                       (\s -> onPrivMsg s (head ps) cp) src
    "JOIN"    -> log $ "*** " ++ who ++ " joined " ++ cp
    "MODE"    -> unless (null ps || head ps == who) $
                   log $ "*** " ++ who ++ " sets mode: " ++ ps' ++ " :" ++ cp
    _         -> unless (length msg == 3 && all isDigit msg) $
                   warn ("Unknown message: " ++ msg ++ " " ++ ps')
  where
    srcName = maybe NoName parseName src
    who     = pretty srcName
    cp      = maybe "" id cp'
    ps'     = unwords ps

log = io . putStrLn

data Name = Name { nickName, userName, fullName :: String } | NoName

class Pretty a where
  pretty :: a -> String

instance Pretty Name where
  pretty (Name n _ _) = n
  pretty NoName       = "<none>"

parseName s = let (nick, rest)  = break (== '!') s
                  (user, rest') = break (== '@') (tail rest)
              in Name nick user (tail rest')

warn = io . putStrLn

onPrivMsg _ _ text = eval text

eval "!uptime"             = uptime >>= privmsg
eval "!quit"               = do write "QUIT" ":Exiting"
                                io (exitWith ExitSuccess)
eval (stripPrefix "!id " -> Just x) = privmsg x
eval ('!':s) = do onCmd <- onCommand `fmap` asks plugins
                  maybe (privmsg "No such command") privmsg (onCmd s)
eval _ = return ()

privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)
 
write s t = do h <- asks socket
               io $ hPrintf h "%s %s\r\n" s t
 
uptime :: Net String
uptime = do
    now  <- io getClockTime
    zero <- asks starttime
    return . pretty $ diffClockTimes now zero
 
instance Pretty TimeDiff where
  pretty td = join . intersperse " " . filter (not . null) . map f $
      [(years          ,"y") ,(months `mod` 12,"m")
      ,(days   `mod` 28,"d") ,(hours  `mod` 24,"h")
      ,(mins   `mod` 60,"m") ,(secs   `mod` 60,"s")]
    where
      secs    = abs $ tdSec td  ; mins   = secs   `div` 60
      hours   = mins   `div` 60 ; days   = hours  `div` 24
      months  = days   `div` 28 ; years  = months `div` 12
      f (i,s) | i == 0    = []
              | otherwise = show i ++ s
 
io :: IO a -> Net a
io = liftIO

-- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
