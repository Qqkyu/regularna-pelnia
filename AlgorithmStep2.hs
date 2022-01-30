module AlgorithmStep2 where
import Utilities

getEmptyNewRegexResult :: String -> [String]
getEmptyNewRegexResult rest =
  if length rest == 0 then
    []
  else
    algorithmStep2Helper rest

-- splits regex into multiple by parentheses - a+(a+e)(a*e)+a*e -> ["a","+","a+e","a*e","+","a*e"]
algorithmStep2Helper :: String -> [String]
algorithmStep2Helper r =
  if length r /= 0 then
    if head r == '+' then
      ["+"] ++ (algorithmStep2Helper $ tail r)
    else
      if length newRegex == 0 then
        getEmptyNewRegexResult rest
      else
        if length rest == 0 then
          if head newRegex == '+' then
            if last newRegex == '+' then
              ["+", tail (init newRegex), "+"]
            else
              ["+", tail newRegex]
          else
            if last newRegex == '+' then
              [init newRegex, "+"]
            else
              [newRegex]
        else
          if head newRegex == '+' then
            if last newRegex == '+' then
              ["+", tail (init newRegex), "+"] ++ algorithmStep2Helper rest
            else
              ["+", tail newRegex] ++ algorithmStep2Helper rest
          else
            if last newRegex == '+' then
              [init newRegex, "+"] ++ algorithmStep2Helper rest
            else
              [newRegex] ++ algorithmStep2Helper rest
  else
    []
  where
    ongoingRegex = if length r == 0 then False else head r == '('
    newRegex =
      if length r /= 0 then
        if ongoingRegex then
          takeWhileOneMore (')'/=) r
        else
          takeWhile ('('/=) r
      else
        ""
    rest =
      if length r /= 0 then
        if ongoingRegex then
          tail (dropWhile (')'/=) (tail r)) -- ?? why tail
        else
          dropWhile ('('/=) (tail r)
      else
        ""

split :: String -> [String] -> [String]
split prev [] = [prev]
split prev (r : rs) =
  if (last prev /= ')' ) && (prev /= "+") && (head r == '(') then
    (splitStringByPlus prev) ++ (split r rs)
  else
    if (last prev == ')') && (r /= "+") && (head r /= '(') then
      if (length $ splitStringByPlus r) == 1 then
        [prev] ++ (split (last (splitStringByPlus r)) rs)
      else
        ([prev] ++ (splitStringByPlus r)) ++ (split (last (splitStringByPlus r)) rs)
    else
      [prev] ++ (split r rs)

splitByPlus :: String -> [String]
splitByPlus s =
  if contains s "+" then
    if contains rest "+" then
      [curR] ++ ["+"] ++ splitByPlus rest
    else
      [curR] ++ ["+"] ++ [rest]
  else
    [s]
  where
    curR = takeWhile ('+'/=) s
    rest = if (dropWhile ('+'/=) s) /= "" then tail (dropWhile ('+'/=) s) else ""

splitByPlusIfNeeded :: [String] -> [String]
splitByPlusIfNeeded [] = []
splitByPlusIfNeeded (r : rs) =
  if head r == '+' || head r == '(' then
    r : splitByPlusIfNeeded rs
  else
    splitByPlus r ++ splitByPlusIfNeeded rs

removeParens :: [String] -> [String]
removeParens [] = []
removeParens (l : ls) =
  if length l < 3 || contains "+" l then
    l : removeParens ls
  else
    if head l == '(' && length l == 3 then
      (init $ tail l) : removeParens ls
    else
      l : removeParens ls

algorithmStep2 :: String -> [String]
algorithmStep2 r = 
  if length basicSplit < 2 then
    if length basicSplit == 0 then
      []
    else
      splitByPlus $ head basicSplit
  else
    splitByPlusIfNeeded (split (head basicSplit) (tail basicSplit))
  where
    basicSplit = removeParens $ algorithmStep2Helper r
