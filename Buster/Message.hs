{-# LANGUAGE FlexibleInstances #-}
module Buster.Message where

import Buster.Misc

data ServerMsg = Away (Maybe String) | Invite Chan | Join Chan
                 | Kick Chan Nick (Maybe String) | Mode Chan [String]
                 | NickChange Nick | Part Chan (Maybe String)
                 | Notice Target String | PrivMsg Target Chat
                 | Topic Chan String | Quit String

data Target = Nick Nick | Chan Chan

type Nick = String
data Chan = (:#) String | (:&) String | (:+) String deriving (Eq, Ord)

instance Pretty Chan where
  pretty ((:#) s) = showString ('#':s)
  pretty ((:&) s) = showString ('&':s)
  pretty ((:+) s) = showString ('+':s)

data Name = Name { nickName, userName, fullName :: String } | NoName

instance Pretty Name where
  pretty (Name n _ _) = showString n
  pretty NoName       = showString "<none>"

instance Pretty (Name, ServerMsg) where
  pretty (nm, msg) = case msg of
    Away m       -> who . maybe (s "is no longer away")
                                (\awayMsg -> s "is away" . paren awayMsg) m
    Invite ch    -> who . s "invited you to " . pretty ch
    Join ch      -> who . s "has joined " . pretty ch
    Kick ch n m  -> who . s "kicked " . s n . s " from " . pretty ch
                    . maybeParen m
    Mode ch ms   -> who . foldl concatWords (s "sets mode:") ms
    NickChange n -> who . s "is now known as " . s n
    Part ch m    -> who . s "has left " . pretty ch . maybeParen m
    Notice t m   -> case t of
      Nick _  -> s "Notice from " . pretty nm . s ": " . s m
      Chan ch -> s "Notice from " . pretty nm . s " (" . pretty ch . s "): "
                                  . s m
    PrivMsg t m  -> case t of
      Nick _  -> s "(PM) " . pretty (nm, m)
      Chan ch -> pretty (nm, m)
    Topic ch m   -> who . s "changed " . pretty ch . s "'s topic to " . s m
    Quit m       -> s " has quit IRC" . paren m
   where
    s               = showString
    who             = s "*** " . pretty nm . s " "
    paren m         = s " (" . s m . s ")"
    maybeParen      = maybe id paren
    concatWords f m = f . (' ':) . s m

data Chat = Chat String | Action String

instance Pretty (Name, Chat) where
  pretty (nm, Chat t)    = ('<':) . pretty nm . ("> " ++) . showString t
  pretty (nm, Action t)  = ("* " ++) . pretty nm . (' ':) . showString t

-- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
