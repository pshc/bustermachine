
import System.IO
import IRC
import Logging
import Misc
import UNIX

config = IrcConfig {
  ircServer   = "irc.opera.com",
  ircPort     = 6667,
  ircChannel  = "#cantide",
  ircNick     = "canti",
  ircUser     = "nono",
  ircFullName = "Buster Machine No. 7"
}

main = do
    dontBuffer stdout; dontBuffer stderr
    unix <- unixPlugin
    logging <- loggingPlugin
    runBot config [unix, logging]

-- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
