
import Buster.IRC
import Buster.Misc
import Buster.Plugin
import System.IO

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
    let lib = "bustermachine-0.1"
    withPlugin lib "UNIX" $ \unix ->
      withPlugin lib "Logging" $ \logging ->
        runBot config (dispatchPlugins [unix, logging])

-- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
