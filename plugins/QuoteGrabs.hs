{-# LANGUAGE ViewPatterns #-}
import Buster.Message
import Buster.Plugin
import Control.Exception (bracket)
import Control.Monad.Trans
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IORef
import Data.List
import Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3)
import System.Posix.Time

main = do recentMsgs <- newIORef Map.empty
          let cmds = [("rq", rq), ("!rq", rq3),
                      ("quote", quote), ("grab", grab recentMsgs)]
          pluginMain $ hybridPlugin cmds (grabTracker recentMsgs) False

rq ps = do r <- liftIO (withDB $ randomQuery 1 (words ps))
           respond (showQuotes r)
rq3 ps = do r <- liftIO (withDB $ randomQuery 3 (words ps))
            respond (showQuotes r)
quote (words -> [nick]) = liftIO (withDB go) >>= respond . showQuotes
  where
    go conn = quickQuery' conn ("SELECT quote FROM quotegrabs WHERE nick = ? "
                                ++ "ORDER BY id DESC LIMIT 1") [toSql nick]

quote _ = respond "You must supply one nickname."

withDB = bracket (connectSqlite3 "data/quotegrabs.sqlite") disconnect

randomQuery :: (IConnection conn) => Int -> [String] -> conn -> IO [[SqlValue]]
randomQuery num nicks conn = quickQuery' conn (q1 ++ q2 ++ q3) ps
  where
    q1 = "SELECT quote FROM quotegrabs "
    q3 = "ORDER BY RANDOM() LIMIT ?"
    (q2, ps) = case nicks of []  -> ("", [toSql num])
                             n:_ -> ("WHERE nick = ? COLLATE NOCASE ",
                                     [toSql n, toSql num])

showQuotes [] = "No quotes found."
showQuotes qs = intercalate " " $ map extract qs
  where
    extract [fromSql -> Just s] = s
    extract _                   = "[invalid quote]"

grab ref (words -> [nick]) | not (null nick) = do
    db <- liftIO $ readIORef ref
    Just grabber <- invoker >>= contextLookup
    addedBy <- pretty grabber
    maybe (respond $ "Nothing to grab from " ++ nick ++ ".")
          (doGrab addedBy) (nick `Map.lookup` db)
  where
    doGrab addedBy _
      | addedBy == nick = respond "Illegal self-grab."
    doGrab addedBy (ui, quote, addedAt) = do
        hostmask <- pretty ui
        let params = [toSql (userNick ui), toSql hostmask, toSql addedBy,
                      toSql addedAt, toSql quote]
        liftIO (withDB $ doInsert params) >>= confirm >>= respond

    doInsert ps c = do r <- run c sql ps
                       if r == 1 then commit c >> return True
                                 else return False

    sql = "INSERT INTO quotegrabs (nick, hostmask, added_by, added_at, "
          ++ "quote) VALUES (?, ?, ?, ?, ?)"

    confirm True  = do liftIO $ modifyIORef ref (nick `Map.delete`)
                       return "Gotcha."
    confirm False = return "Couldn't grab due to database error."

grab _ _ = respond "Please specify one nickname."

grabTracker ref (user, PrivMsg (Channel ch) chat) = do
    userInfo <- contextLookup user
    quote <- pretty (user, chat)
    maybe (return ()) (liftIO . trackGrab quote) userInfo
  where
    trackGrab q ui = do now <- epochTime
                        let t = read (show now) :: Int
                        modifyIORef ref $ Map.insert (userNick ui) (ui, q, t)
grabTracker _ _ = return ()

-- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
