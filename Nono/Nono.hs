
import Buster.IRC
import Buster.Misc
import Buster.Machine
import Config
import System.IO

main = do
    dontBuffer stdout; dontBuffer stderr
    processor <- setupMachine activePlugins
    runBot config processor

-- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
