{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Buster.Message where

import Buster.Misc

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
  pretty ((:#) s) = showString ('#':s)
  pretty ((:&) s) = showString ('&':s)
  pretty ((:+) s) = showString ('+':s)

data User = UserID Int deriving (Eq, Ord, Read, Show)
data UserInfo = UserInfo { userNick, userUser, userHost :: String }
                deriving (Read, Show)

-- TEMP
instance Pretty User where
  pretty (UserID u) = showString ("User #") . shows u

data Target = User User | Chan Chan deriving (Read, Show)

instance Pretty Target where
  pretty (User u)  = pretty u
  pretty (Chan ch) = pretty ch

data Priv = Op | Voice | Regular deriving (Read, Show)

instance Pretty ServerMsg where
  pretty (src, msg) = case msg of
    Away m       -> who . maybe (s "is no longer away")
                                (\awayMsg -> s "is away" . paren awayMsg) m
    Invite ch    -> who . s "invited you to " . pretty ch
    Join ch      -> who . s "has joined " . pretty ch
    Kick _ u m   -> s "*** " . pretty u . s " was kicked by " . pretty src
                    . maybeParen m
    Mode _ ms    -> who . foldl concatWords (s "sets mode:") ms
    NickChange n -> who . s "is now known as " . s n
    Part ch m    -> who . s "has left " . pretty ch . maybeParen m
    Notice t m   -> case t of
      User _  -> s "Notice from " . pretty src . s ": " . s m
      Chan ch -> s "Notice from " . pretty src . s " (" . pretty ch . s "): "
                                  . s m
    PrivMsg t m  -> case t of
      User _ -> s "(PM) " . pretty (src, m)
      Chan _ -> pretty (src, m)
    Topic _ m    -> who . s "changes topic to \"" . s m . s "\""
    Quit m       -> s " has quit IRC" . paren m
   where
    s               = showString
    who             = s "*** " . pretty src . s " "
    paren m         = s " (" . s m . s ")"
    maybeParen      = maybe id paren
    concatWords f m = f . (' ':) . s m

data Chat = Chat String | Action String deriving (Read, Show)

instance Pretty (User, Chat) where
  pretty (u, Chat t)    = ('<':) . pretty u . ("> " ++) . showString t
  pretty (u, Action t)  = ("* " ++) . pretty u . (' ':) . showString t

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
