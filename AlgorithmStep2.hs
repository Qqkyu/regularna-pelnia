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
          tail (dropWhile (')'/=) (tail r))
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
      ([prev] ++ (splitStringByPlus r)) ++ (split (last (splitStringByPlus r)) rs)
    else
      [prev] ++ (split r rs)

algorithmStep2 :: String -> [String]
algorithmStep2 r = 
  if length basicSplit < 2 then
    basicSplit
  else
    split (head basicSplit) (tail basicSplit)
  where
    basicSplit = algorithmStep2Helper r
