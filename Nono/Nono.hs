
import Buster.IRC
import Buster.Misc
import Buster.Plugin
import Config
import System.IO

main = do
    dontBuffer stdout; dontBuffer stderr
    let lib = "bustermachine-0.1"
    withPlugin lib "UNIX" $ \unix ->
      withPlugin lib "Logging" $ \logging ->
        withPlugin lib "Identity" $ \identity ->
          runBot config (dispatchPlugins [unix, logging, identity])

-- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
