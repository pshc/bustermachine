module Buster.Internal where

import Buster.Message
import Buster.Misc
import Buster.IRC
import Control.Monad.Trans
import Data.Map (Map)
import Data.Set (Set)
import System.IO

data PluginResp = ClientMsg IRCMsg | ConfigLookup String | UsersQuery Channel
                  | ChansQuery | UserLookup User | EndResponse
                  deriving (Read, Show)

data MachineReq = ReqProcess ServerMsg | ReqCommand IString User Target String
                  | ReqConfig String (Maybe String)
                  | ReqUsers Channel (Maybe (Map User Priv))
                  | ReqChans (Map Channel ChannelState)
                  | ReqUser User (Maybe UserInfo)
                  deriving (Read, Show)

data API = API {
    apiCommandSet :: Set IString,
    apiHasProcessor :: Bool
} deriving (Read, Show)

send :: (MonadIO m, Show a) => Handle -> a -> m ()
send h = liftIO . (>> hFlush h) . hPutStrLn h . show

recv :: (MonadIO m, Read a) => Handle -> m (Either String a)
recv h = do l <- liftIO $ hGetLine h
            return $ case reads l of [(a, "")] -> Right a
                                     _         -> Left l
