module Utilities where

contains :: String -> String -> Bool
contains str subStr
  | length str < length subStr         = False
  | length str == length subStr        = str == subStr
  | take (length subStr) str == subStr = True
  | otherwise                          = contains (tail str) subStr
