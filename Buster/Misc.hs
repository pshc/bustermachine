module Buster.Misc where

import Control.Monad.Trans
import Data.Char
import System.IO

stripLeft = dropWhile (== ' ')

stripIPrefix :: String -> String -> Maybe String
stripIPrefix [] ys = Just ys
stripIPrefix (x:xs) (y:ys) | toLower x == toLower y = stripIPrefix xs ys
stripIPrefix _ _ = Nothing

lower = map toLower

newtype IString = IString String deriving (Read, Show)
instance Eq IString where
  IString a == IString b = lower a == lower b
instance Ord IString where
  IString a `compare` IString b = lower a `compare` lower b

dontBuffer = flip hSetBuffering NoBuffering

isChan (x:_) = (x == '#' || x == '&' || x == '+')
isChan _     = False

-- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
