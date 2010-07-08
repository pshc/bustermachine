{-# LANGUAGE FlexibleInstances #-}
module Buster.Message where

import Buster.Misc

data ServerMsg = Away (Maybe String) | Invite Channel | Join Channel
                 | Kick Channel Nick (Maybe String) | Mode Channel [String]
                 | Nick Nick | Part Channel (Maybe String) | Notice Nick String
                 | Privmsg ChatMsg | Chanmsg Channel ChatMsg
                 | Topic Channel String | Quit String

type Channel = String
type Nick = String

instance Pretty (Name, ServerMsg) where
  pretty (nm, msg) = case msg of
    Away m      -> who . maybe (s "is no longer away")
                               (\awayMsg -> s "is away" . paren awayMsg) m
    Invite ch   -> who . s "invited you to " . s ch
    Join ch     -> who . s "has joined " . s ch
    Kick ch n m -> who . s "kicked " . s n . s " from " . s ch . maybeParen m
    Mode ch ms  -> who . foldl concatWords (s "sets mode:") ms
    Nick n      -> who . s "is now known as " . s n
    Part ch m   -> who . s "has left " . s ch . maybeParen m
    Notice n m  -> s "Notice from " . s n . s ": " . s m
    Privmsg m   -> s "(PM) " . pretty (nm, m)
    Chanmsg _ m -> pretty (nm, m)
    Topic ch m  -> who . s "changed " . s ch . s "'s topic to " . s m
    Quit m      -> s " has quit IRC" . paren m
   where
    s               = showString
    who             = s "*** " . pretty nm . s " "
    paren m         = s " (" . s m . s ")"
    maybeParen      = maybe id paren
    concatWords f m = f . (' ':) . s m

data ChatMsg = ChatMsg String | Action String

instance Pretty (Name, ChatMsg) where
  pretty (nm, ChatMsg t) = ('<':) . pretty nm . ("> " ++) . showString t
  pretty (nm, Action t)  = ("* " ++) . pretty nm . (' ':) . showString t

data Name = Name { nickName, userName, fullName :: String } | NoName

instance Pretty Name where
  pretty (Name n _ _) = showString n
  pretty NoName       = showString "<none>"

-- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
