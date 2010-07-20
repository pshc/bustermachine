
import Buster.Plugin
import Control.Exception (bracket)
import Data.Time.Clock
import Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3)

main = pluginMain $ commandPlugin [("rq", rq)]

withDB = bracket (connectSqlite3 "data/quotegrabs.sqlite") disconnect

rq ch nm = showQuote =<< io (withDB randomQuery)
  where
    randomQuery conn = quickQuery' conn (q1 ++ q2 ++ q3) ps
    (q1, q3) = ("SELECT quote FROM quotegrabs ", "ORDER BY RANDOM() LIMIT 1")
    (q2, ps) = case words nm of []    -> ("", [])
                                (n:_) -> ("WHERE nick = ? ", [toSql n])
    showQuote [[q]] = respondChat ch $ maybe "Bad quote." id (fromSql q)
    showQuote _     = respondChat ch $ "No quotes found."

-- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent: