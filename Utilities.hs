module Utilities where

findLhs :: String -> String
findLhs = takeWhile (/='+')

findRhs :: String -> String
findRhs r = takeWhile (/='+') (tail (dropWhile (/='+') r))

findRest :: String -> String
findRest r = dropWhile (/='+') (tail (dropWhile (/='+') r))

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

-- extract number from parts like M[n], P[n] (works also for N[n1,n2])
findNumber :: String -> String
findNumber p =
  tail (takeWhile (\x -> x /= ']') (dropWhile (\x -> x /= '[') p))

-- extract number from parts like M[n], P[n] and convert to int (works also for N[n1,n2])
findNumberInt :: String -> Int
findNumberInt p =
  read (tail (takeWhile (\x -> x /= ']') (dropWhile (\x -> x /= '[') p))) :: Int

findSmallestFromListHelper :: String -> String -> String
findSmallestFromListHelper prev list =
  if length list == 0 then
    prev
  else
    if (read prev :: Int) < (read cur :: Int) then
      findSmallestFromListHelper prev newList
    else
      findSmallestFromListHelper cur newList
  where
    cur = takeWhile (\x -> x /= ',') (tail list)
    newList = dropWhile (\x -> x /= ',') (tail list)

-- extract smallest number from list - ("[1,2,3]" -> "1")
findSmallestFromList :: String -> String
findSmallestFromList p = findSmallestFromListHelper first newList
  where
    list = findNumber p
    first = takeWhile (\x -> x /= ',') list
    newList = dropWhile (\x -> x /= ',') list

addNumToList :: String -> String -> String
addNumToList num list = 
  if length list == 0 then
    ""
  else
    if rest /= "" then
      (show ((read num :: Int) + (read cur :: Int))) ++ "," ++ rest
    else
      show ((read num :: Int) + (read cur :: Int))
  where
    cur = takeWhile (\x -> x /= ',') list
    newList = drop 1 (dropWhile (\x -> x /= ',') list)
    rest = addNumToList num newList

addListsHelper :: String -> String -> String
addListsHelper lhsList rhsList =
  if length lhsList == 0 then
    ""
  else
    if rest /= "" then
      (addNumToList cur rhsList) ++ "," ++ rest
    else
      addNumToList cur rhsList
  where
    cur = takeWhile (\x -> x /= ',') lhsList
    newLhsList = drop 1 (dropWhile (\x -> x /= ',') lhsList)
    rest = addListsHelper newLhsList rhsList

addLists :: String -> String -> String
addLists lhs rhs = addListsHelper lhsList rhsList
  where
    lhsList = findNumber lhs
    rhsList = findNumber rhs
