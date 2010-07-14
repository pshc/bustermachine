{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Buster.Message where

import Buster.Misc

data IRCMsg = Away (Maybe String) | Invite Chan | Join Chan
              | Kick Chan Nick (Maybe String) | Mode Chan [String]
              | NickChange Nick | Part Chan (Maybe String)
              | Notice Target String | PrivMsg Target Chat
              | Topic Chan String | Quit String
              deriving (Read, Show)

type ServerMsg = (Name, IRCMsg)

type Nick = String
data Chan = (:#) String | (:&) String | (:+) String
            deriving (Eq, Ord, Read, Show)

instance Pretty Chan where
  pretty ((:#) s) = showString ('#':s)
  pretty ((:&) s) = showString ('&':s)
  pretty ((:+) s) = showString ('+':s)

data Target = Nick Nick | Chan Chan deriving (Read, Show)

instance Pretty Target where
  pretty (Nick n)  = showString n
  pretty (Chan ch) = pretty ch

data Name = Name { nickName, userName, fullName :: String } | NoName
            deriving (Read, Show)
data Priv = Op | Voice | Regular deriving (Read, Show)

instance Pretty Name where
  pretty (Name n _ _) = showString n
  pretty NoName       = showString "<none>"

instance Pretty ServerMsg where
  pretty (nm, msg) = case msg of
    Away m       -> who . maybe (s "is no longer away")
                                (\awayMsg -> s "is away" . paren awayMsg) m
    Invite ch    -> who . s "invited you to " . pretty ch
    Join ch      -> who . s "has joined " . pretty ch
    Kick _ n m   -> s "*** " . s n . s " was kicked by " . pretty nm
                    . maybeParen m
    Mode _ ms    -> who . foldl concatWords (s "sets mode:") ms
    NickChange n -> who . s "is now known as " . s n
    Part ch m    -> who . s "has left " . pretty ch . maybeParen m
    Notice t m   -> case t of
      Nick _  -> s "Notice from " . pretty nm . s ": " . s m
      Chan ch -> s "Notice from " . pretty nm . s " (" . pretty ch . s "): "
                                  . s m
    PrivMsg t m  -> case t of
      Nick _  -> s "(PM) " . pretty (nm, m)
      Chan ch -> pretty (nm, m)
    Topic _ m    -> who . s "changes topic to \"" . s m . s "\""
    Quit m       -> s " has quit IRC" . paren m
   where
    s               = showString
    who             = s "*** " . pretty nm . s " "
    paren m         = s " (" . s m . s ")"
    maybeParen      = maybe id paren
    concatWords f m = f . (' ':) . s m

data Chat = Chat String | Action String deriving (Read, Show)

instance Pretty (Name, Chat) where
  pretty (nm, Chat t)    = ('<':) . pretty nm . ("> " ++) . showString t
  pretty (nm, Action t)  = ("* " ++) . pretty nm . (' ':) . showString t

filterByChan :: (Nick -> Bool) -> ServerMsg -> Chan -> Bool
filterByChan inChan (nm, msg) ch = case msg of
    Away _             -> sameChan
    Join c             -> c == ch
    Kick c _ _         -> c == ch
    Mode c _           -> c == ch
    NickChange _       -> sameChan
    Part c _           -> c == ch
    --Notice  (Chan c) _ -> c == ch
    PrivMsg (Chan c) _ -> c == ch
    Topic c _          -> c == ch
    Quit _             -> sameChan
    _                  -> False
  where
    sameChan = inChan (pretty nm "")

-- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
