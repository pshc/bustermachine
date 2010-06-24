module Plugin where

data Response = ChannelMessage String

data API = Plugin {
    regCommands :: [(String, String -> IO [Response])]
}

plugin = Plugin {
    regCommands = [("what", \s -> return [ChannelMessage ("that " ++ s)])]
}

resource = plugin

-- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
