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
        let filename = dir ++ "/" ++ chan ++ "." ++ dateStr t ++ ".log"
        io $ writeLine filename
      where
        inChannel = filterByChan (`Map.member` chanUsers cs) False msg ch
        writeLine fnm = do h <- openFile fnm AppendMode
                           hSetBinaryMode h True
                           hPutStrLn h $ timeStr t ++ line
                           hClose h

format' = formatTime defaultTimeLocale . iso8601DateFormat
dateStr = format' Nothing
timeStr = format' (Just "%T  ")

instance Pretty ServerMsg where
  pretty (src, msg) = do who <- pretty src
                         ($ "") `fmap` prettyMsg (src, msg) (showString who)

prettyMsg :: (Context m) => ServerMsg -> ShowS -> m ShowS
prettyMsg (src, msg) who = case msg of
    Away m       -> meta $ maybe (s "is no longer away")
                                 (\awayMsg -> s "is away" . paren awayMsg) m
    Invite ch    -> do chan <- pretty ch
                       meta $ s "invited you to " . s chan
    Join ch      -> do chan <- pretty ch
                       meta $ s "has joined " . s chan
    Kick _ u m   ->  do kickee <- pretty u
                        return $ s "*** " . s kickee . s " was kicked by "
                                          . who . maybeParen m
    Mode _ ms    -> meta $ foldl concatWords (s "sets mode:") ms
    NickChange n -> meta $ s "is now known as " . s n
    Part ch m    -> do chan <- pretty ch
                       meta $ s "has left " . s chan . maybeParen m
    Notice t m   -> case t of
      User _  -> return $ s "Notice from " . who . s ": " . s m
      Chan ch -> do chan <- pretty ch
                    return $ s "Notice from " . who . s " (" . s chan
                                              . s "): " . s m
    PrivMsg t m  -> case t of
      User _ -> do pm <- pretty (src, m)
                   return $ s "(PM) " . s pm
      Chan _ -> s `fmap` pretty (src, m)
    Topic _ m    -> meta $ s "changes topic to \"" . s m . s "\""
    Quit m       -> meta $ s "has quit IRC" . paren m
   where
    s               = showString
    meta f          = return $ s "*** " . who . (' ':) . f
    paren m         = s " (" . s m . s ")"
    maybeParen      = maybe id paren
    concatWords f m = f . (' ':) . s m

-- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
