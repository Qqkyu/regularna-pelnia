module AlgorithmStep0 where
import Utilities

removeSpaces :: String -> String
removeSpaces [] = []
removeSpaces (a : as)
  | a == ' '  = removeSpaces as
  | otherwise = a : removeSpaces as

findExprTillPlus :: String -> String
findExprTillPlus (a : []) = [a]
findExprTillPlus (a : b : []) = [a] ++ [b]
findExprTillPlus (prev : cur : rest) =
  if prev == ')' && cur == '+' then
    [prev] ++ [cur]
  else
    [prev] ++ findExprTillPlus (cur : rest)

findFirstNonEmpty :: String -> String
findFirstNonEmpty r =
  if head nextR == '(' then
    if (last $ init $ findExprTillPlus r) == ')' then
      init (findExprTillPlus r)
    else
      init (findExprTillPlus r) ++ ")"
  else
    takeWhile ('+'/=) r
  where
    nextR = takeWhile (\x -> x /= '+' && x /= ')') r

findFirst :: String -> String
findFirst r =
  if length r == 0 then
    ""
  else
    findFirstNonEmpty r

findFirstAfterParens :: String -> (String,String)
findFirstAfterParens r =
  if length nextR == 0 then
    ("", r)
  else
    (findFirst $ tail nextR, drop ((length $ findFirst $ tail nextR) + ((length r) - (length nextR)) + 1) r)
  where
    nextR = dropWhile (')'/=) r

findFirstInParens :: String -> String
findFirstInParens = tail . (takeWhile ('+'/=))

flattenParens :: String -> String -> String
flattenParens prevR rest =
  if ((length . (filter ('+'==))) parens) > 1 then
    prevR ++ nextR ++ findFirstInParens rest ++ "+" ++ newParens ++ nextR
  else
    prevR ++ rest
  where
    nextR = fst (findFirstAfterParens rest)
    parens = (takeWhile (\x -> x /= ')') rest) ++ ")"
    newParens = "(" ++ tail (dropWhile (\x -> x /= '+') ((takeWhile (\x -> x /= ')') rest) ++ ")"))

flattenStructure :: String -> String
flattenStructure r =
  if length r < 6 then
    r
  else
    if head nextR == '(' then
      if (flattenParens curR nextR) == curR ++ nextR then
        takeWhile (')'/=) nextR ++ ")" ++ fst (findFirstAfterParens nextR) ++ flattenStructure (snd (findFirstAfterParens nextR))
      else
        flattenStructure (flattenParens curR nextR)
    else
      if head nextR == '+' && length nextR > 1 && (head $ tail nextR) == '(' then
        curR ++ "+" ++ flattenStructure (tail (flattenParens "+" (tail nextR)))
      else
        curR ++ flattenStructure (nextR)
  where
    curR = takeWhile (\x -> x /= '+' && x /= '(') r
    nextR = dropWhile (\x -> x /= '+' && x /= '(') r

algorithmStep0 :: String -> String
algorithmStep0 = flattenStructure . removeSpaces
