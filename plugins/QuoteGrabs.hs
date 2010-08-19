{-# LANGUAGE ViewPatterns #-}
import Buster.Message
import Buster.Plugin
import Control.Exception (bracket)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IORef
import Data.List
import Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3)
import System.Posix.Time

main = do recentMsgs <- newIORef Map.empty
          pluginMain $ hybridPlugin [("rq", rq), ("!rq", rq3),
                       ("grab", grab recentMsgs)] (grabTracker recentMsgs)

rq _ ch ps = do r <- io (withDB $ randomQuery 1 (words ps))
                respondChat ch (showQuotes r)
rq3 _ ch ps = do r <- io (withDB $ randomQuery 3 (words ps))
                 respondChat ch (showQuotes r)

withDB = bracket (connectSqlite3 "data/quotegrabs.sqlite") disconnect

randomQuery :: (IConnection conn) => Int -> [String] -> conn -> IO [[SqlValue]]
randomQuery num nicks conn = quickQuery' conn (q1 ++ q2 ++ q3) ps
  where
    q1 = "SELECT quote FROM quotegrabs "
    q3 = "ORDER BY RANDOM() LIMIT ?"
    (q2, ps) = case nicks of []  -> ("", [toSql num])
                             n:_ -> ("WHERE nick = ? ", [toSql n, toSql num])

showQuotes [] = "No quotes found."
showQuotes qs = intercalate " " $ map extract qs
  where
    extract [fromSql -> Just s] = s
    extract _                   = "[invalid quote]"

grab ref grabber ch (words -> [nick]) | not (null nick) = do
    db <- io $ readIORef ref
    Just grabberUi <- contextLookup grabber
    addedBy <- fmap ($ "") (pretty grabberUi)
    maybe (respondChat ch "No stored messages by anyone with that nick.")
          (doGrab addedBy) (nick `Map.lookup` db)
  where
    doGrab addedBy _
      | addedBy == nick = respondChat ch "Illegal self-grab."
    doGrab addedBy (ui, quote, addedAt) = do
        hostmask <- ($ "") `fmap` pretty ui
        let params = [toSql (userNick ui), toSql hostmask, toSql addedBy,
                      toSql addedAt, toSql quote]
        ok <- io $ withDB (\c -> do res <- run c insert params
                                    if res == 1 then commit c >> return True
                                                else return False)
        respondChat ch (confirm ok)
    insert = "INSERT INTO quotegrabs (nick, hostmask, added_by, added_at, "
             ++ "quote) VALUES (?, ?, ?, ?, ?)"
    confirm True  = "Gotcha."
    confirm False = "Couldn't grab due to database error."
grab _ _ ch _ = respondChat ch "Please specify one nickname."

grabTracker ref (user, PrivMsg (Chan ch) chat) = do
    userInfo <- contextLookup user
    quote <- ($ "") `fmap` pretty (user, chat)
    maybe (return ()) (io . trackGrab quote) userInfo
  where
    trackGrab q ui = do now <- epochTime
                        let t = read (show now) :: Int
                        modifyIORef ref $ Map.insert (userNick ui) (ui, q, t)
grabTracker _ _ = return ()

-- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
