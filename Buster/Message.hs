{-# LANGUAGE FlexibleInstances #-}
module Buster.Message where

import Buster.Misc
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

class (Functor m, Monad m) => Context m where
  contextLookup :: User -> m (Maybe UserInfo)

--instance (MonadTrans t, Context m, Monad (t m)) => Context (t m) where
--  contextLookup = lift . contextLookup

class Pretty a where
  pretty :: (Context m) => a -> m String

data IRCMsg = Away (Maybe String) | Invite Channel | Join Channel
              | Kick Channel User (Maybe String) | Mode Channel [String]
              | NickChange String | Part Channel (Maybe String)
              | Notice Target String | PrivMsg Target Chat
              | Topic Channel String | Quit String
              deriving (Read, Show)

type ServerMsg = (User, IRCMsg)

data Channel = (:#) String | (:&) String | (:+) String
            deriving (Eq, Ord, Read, Show)

instance Pretty Channel where
  pretty ((:#) s) = return ('#':s)
  pretty ((:&) s) = return ('&':s)
  pretty ((:+) s) = return ('+':s)

data User = UserID Int deriving (Eq, Ord, Read, Show)
data UserInfo = UserInfo { userNick, userUser, userHost :: String }
                deriving (Read, Show)

instance Pretty User where
  pretty u = do ui <- contextLookup u
                return $ maybe "???" userNick ui

instance Pretty UserInfo where
  pretty (UserInfo n u h) = return $ n ++ "!" ++ u ++ "@" ++ h

data UsersState = UsersState { selfUser :: User,
                               users :: Map User UserInfo,
                               userNicks :: Map String User,
                               idCtr :: Int }

data Target = User User | Channel Channel deriving (Read, Show)

instance Pretty Target where
  pretty (User u)  = pretty u
  pretty (Channel ch) = pretty ch

data Priv = Op | Voice | Regular deriving (Read, Show)

data Chat = Chat String | Action String deriving (Read, Show)

instance Pretty (User, Chat) where
  pretty (u, c) = pretty u >>= \nick -> case c of
    Chat t   -> return $ '<':nick ++ "> " ++ t
    Action t -> return $ "* " ++ nick ++ " " ++ t

filterByChan :: (User -> Bool) -> Bool -> ServerMsg -> Channel -> Bool
filterByChan inChan extra (src, msg) ch = case msg of
    Away _             -> extra -- && inChan src
    Join c             -> c == ch
    Kick c _ _         -> c == ch
    Mode c _           -> c == ch
    NickChange _       -> True -- inChan src
    Part c _           -> c == ch
    Notice  (Channel c) _ -> extra && c == ch
    PrivMsg (Channel c) _ -> c == ch
    Topic c _          -> c == ch
    Quit _             -> True -- inChan src
    _                  -> False

-- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
