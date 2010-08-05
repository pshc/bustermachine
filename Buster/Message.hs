{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances #-}
module Buster.Message where

import Buster.Misc
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

class (Monad m) => Context m where
  contextLookup :: User -> m (Maybe String)

--instance (MonadTrans t, Context m, Monad (t m)) => Context (t m) where
--  contextLookup = lift . contextLookup

class Pretty a where
  pretty :: (Context m) => a -> m ShowS

data IRCMsg = Away (Maybe String) | Invite Chan | Join Chan
              | Kick Chan User (Maybe String) | Mode Chan [String]
              | NickChange String | Part Chan (Maybe String)
              | Notice Target String | PrivMsg Target Chat
              | Topic Chan String | Quit String
              deriving (Read, Show)

type ServerMsg = (User, IRCMsg)

data Chan = (:#) String | (:&) String | (:+) String
            deriving (Eq, Ord, Read, Show)

instance Pretty Chan where
  pretty ((:#) s) = return $ showString ('#':s)
  pretty ((:&) s) = return $ showString ('&':s)
  pretty ((:+) s) = return $ showString ('+':s)

data User = UserID Int deriving (Eq, Ord, Read, Show)
data UserInfo = UserInfo { userNick, userUser, userHost :: String }
                deriving (Read, Show)

instance Pretty User where
  pretty u = do nm <- contextLookup u
                return $ maybe ("???" ++) showString nm

data UsersState = UsersState { selfUser :: User,
                               users :: Map User UserInfo,
                               userNicks :: Map String User,
                               idCtr :: Int }

data Target = User User | Chan Chan deriving (Read, Show)

instance Pretty Target where
  pretty (User u)  = pretty u
  pretty (Chan ch) = pretty ch

data Priv = Op | Voice | Regular deriving (Read, Show)

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
    Quit m       -> meta $ s " has quit IRC" . paren m
   where
    s               = showString
    meta f          = return $ s "*** " . who . (' ':) . f
    paren m         = s " (" . s m . s ")"
    maybeParen      = maybe id paren
    concatWords f m = f . (' ':) . s m

data Chat = Chat String | Action String deriving (Read, Show)

instance Pretty (User, Chat) where
  pretty (u, c) = pretty u >>= \nick -> case c of
    Chat t   -> return $ ('<':) . nick . ("> " ++) . showString t
    Action t -> return $ ("* " ++) . nick . (' ':) . showString t

filterByChan :: (User -> Bool) -> Bool -> ServerMsg -> Chan -> Bool
filterByChan inChan extra (src, msg) ch = case msg of
    Away _             -> extra -- && inChan src
    Join c             -> c == ch
    Kick c _ _         -> c == ch
    Mode c _           -> c == ch
    NickChange _       -> True -- inChan src
    Part c _           -> c == ch
    Notice  (Chan c) _ -> extra && c == ch
    PrivMsg (Chan c) _ -> c == ch
    Topic c _          -> c == ch
    Quit _             -> True -- inChan src
    _                  -> False

-- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
