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

checkStep2 :: Char -> String -> String -> Bool
checkStep2 prevChar prevString [] = True
checkStep2 prevChar prevString (curChar : nextString) =
  if prevChar == 'a' && curChar == '*' then
    nextStep && not (isLiteralConcatenated (init prevString) nextString)
  else
    nextStep
  where
    nextStep = checkStep2 curChar (prevString ++ [curChar]) nextString

doesAlgorithmStep2Pass :: String -> Bool
doesAlgorithmStep2Pass r
  | length r <= 2 = True
  | otherwise     = checkStep2 (head r) [(head r)] (tail r)

mapRegex :: String -> String
mapRegex r =
    
  where
    curRegex = takeWhile (\x -> x /= '*' && x /= '+') r
    nextRegex = tail (dropWhile (\x -> x /= '*' && x /= '+') r)

algorithmStep3 :: [String] -> [String]
algorithmStep3 = map mapRegex
