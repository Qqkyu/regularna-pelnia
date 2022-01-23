module Utilities where

isForwardConcatenated :: String -> Bool
isForwardConcatenated [] = False
isForwardConcatenated (a : as)
  | a /= 'a' && a /= 'e' = False
  | a == 'a'              = True
  | otherwise            = isForwardConcatenated as

isBackwardConcatenated :: String -> Bool
isBackwardConcatenated a = isForwardConcatenated $ reverse a

isLiteralConcatenated :: String -> String -> Bool
isLiteralConcatenated prevString nextString =
  (isForwardConcatenated nextString) || (isBackwardConcatenated prevString)

isConcatenatedWithLiteral :: Char -> String -> String -> Bool
isConcatenatedWithLiteral prevChar prevString [] = True
isConcatenatedWithLiteral prevChar prevString (curChar : nextString) =
  if prevChar == 'a' && curChar == '*' then
    nextStep && not (isLiteralConcatenated (init prevString) nextString)
  else
    nextStep
  where
    nextStep = isConcatenatedWithLiteral curChar (prevString ++ [curChar]) nextString

-- takes simple regex - without parentheses and '+'
generatesFullLanguage :: String -> Bool
generatesFullLanguage r
  | length r <= 2 = True
  | otherwise     = isConcatenatedWithLiteral (head r) [(head r)] (tail r)

contains :: String -> String -> Bool
contains str subStr
  | length str < length subStr         = False
  | length str == length subStr        = str == subStr
  | take (length subStr) str == subStr = True
  | otherwise                          = contains (tail str) subStr

countLiteralOccurences :: String -> Int
countLiteralOccurences = (length . (filter (=='a')))

countLiteralKleeneStarOccurencesHelper :: Char -> String -> Int
countLiteralKleeneStarOccurencesHelper prevChar [] = 0
countLiteralKleeneStarOccurencesHelper prevChar (curChar : rest) =
  if prevChar == 'a' && curChar == '*' then
    1 + nextCount
  else
    nextCount
  where
    nextCount = countLiteralKleeneStarOccurencesHelper curChar rest

countLiteralKleeneStarOccurences :: String -> Int
countLiteralKleeneStarOccurences s
  | length s < 2 = 0
  | otherwise    = countLiteralKleeneStarOccurencesHelper (head s) (tail s)
