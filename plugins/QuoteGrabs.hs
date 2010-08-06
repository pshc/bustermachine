{-# LANGUAGE ViewPatterns #-}
import Buster.Plugin
import Control.Exception (bracket)
import Data.List
import Data.Time.Clock
import Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3)

main = pluginMain $ commandPlugin [("rq", rq), ("!rq", rq3)]

rq ch ps = do r <- io (withDB $ randomQuery 1 (words ps))
              respondChat ch (showQuotes r)
rq3 ch ps = do r <- io (withDB $ randomQuery 3 (words ps))
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

-- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
