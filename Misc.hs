module Misc where

import Data.Char

class Pretty a where
  pretty :: a -> ShowS

stripLeft = dropWhile (== ' ')

stripIPrefix :: String -> String -> Maybe String
stripIPrefix [] ys = Just ys
stripIPrefix (x:xs) (y:ys) | toLower x == toLower y = stripIPrefix xs ys
stripIPrefix _ _ = Nothing

-- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
