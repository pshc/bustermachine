module Plugin where

data API = Plugin {
    onCommand :: String -> Maybe String
}

plugin = Plugin {
    onCommand = \x -> Just x
}

resource = plugin

-- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
