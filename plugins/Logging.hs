{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
import Buster.IRC
import Buster.Message
import Buster.Plugin
import Control.Monad.State
import qualified Data.Map as Map
import Data.Time
import System.Directory
import System.Locale
import System.IO

main = do let dir = "logs"
          doesDirectoryExist dir >>= (`unless` createDirectory dir)
          pluginMain $ processorPlugin (logStub dir)
  where
    logStub dir msg = queryChans >>= logger dir msg
 
logger dir msg chs = do
    now <- io getZonedTime
    line <- pretty msg
    mapM_ (go now line) (Map.assocs chs)
  where
    go t line (ch, cs) = when inChannel $ do
        chan <- pretty ch
        let filename = dir ++ "/" ++ chan "" ++ "." ++ dateStr t ++ ".log"
        io $ writeLine filename
      where
        inChannel = filterByChan (`Map.member` chanUsers cs) False msg ch
        writeLine fnm = do h <- openFile fnm AppendMode
                           hSetBinaryMode h True
                           hPutStr h $ (timeStr t ++) . line $ "\n"
                           hClose h

format' = formatTime defaultTimeLocale . iso8601DateFormat
dateStr = format' Nothing
timeStr = format' (Just "%T  ")

instance Pretty ServerMsg where
  pretty (src, msg) = pretty src >>= prettyMsg (src, msg)

prettyMsg :: (Context m) => ServerMsg -> ShowS -> m ShowS
prettyMsg (src, msg) who = case msg of
    Away m       -> meta $ maybe (s "is no longer away")
                                 (\awayMsg -> s "is away" . paren awayMsg) m
    Invite ch    -> do chan <- pretty ch
                       meta $ s "invited you to " . chan
    Join ch      -> do chan <- pretty ch
                       meta $ s "has joined " . chan
    Kick _ u m   ->  do kickee <- pretty u
                        return $ s "*** " . kickee . s " was kicked by " . who
                                          . maybeParen m
    Mode _ ms    -> meta $ foldl concatWords (s "sets mode:") ms
    NickChange n -> meta $ s "is now known as " . s n
    Part ch m    -> do chan <- pretty ch
                       meta $ s "has left " . chan . maybeParen m
    Notice t m   -> case t of
      User _  -> return $ s "Notice from " . who . s ": " . s m
      Chan ch -> do chan <- pretty ch
                    return $ s "Notice from " . who . s " (" . chan . s "): "
                                              . s m
    PrivMsg t m  -> case t of
      User _ -> do pm <- pretty (src, m)
                   return $ s "(PM) " . pm
      Chan _ -> pretty (src, m)
    Topic _ m    -> meta $ s "changes topic to \"" . s m . s "\""
    Quit m       -> meta $ s "has quit IRC" . paren m
   where
    s               = showString
    meta f          = return $ s "*** " . who . (' ':) . f
    paren m         = s " (" . s m . s ")"
    maybeParen      = maybe id paren
    concatWords f m = f . (' ':) . s m

instance Pretty (User, Chat) where
  pretty (u, c) = pretty u >>= \nick -> case c of
    Chat t   -> return $ ('<':) . nick . ("> " ++) . showString t
    Action t -> return $ ("* " ++) . nick . (' ':) . showString t

-- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
