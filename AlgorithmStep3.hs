module AlgorithmStep3 where
import AlgorithmStep0
import Utilities

mapSinglePart :: String -> String
mapSinglePart r =
  if algorithmStep0 r then  -- r contains a*
    if generatesFullLanguage r then -- 4th type language
      "X"
    else
      "M[" ++ (show $ ((countLiteralOccurences r) - countLiteralKleeneStarOccurences r) - 1) ++ "]"
  else
    if r == "e" then
      "0"
    else
      "P[" ++ (show $ countLiteralOccurences r) ++ "]"

mapRegex :: String -> String
mapRegex r =
  if r == "+" then
    "+"
  else
    if shouldMapNextRegex then
      (mapSinglePart curRegex) ++ ['+'] ++ (mapRegex (tail nextRegex))
    else
      mapSinglePart curRegex ++ nextRegex
  where
    curRegex = takeWhile (\x -> x /= '+') r
    nextRegex = dropWhile (\x -> x /= '+') r
    shouldMapNextRegex = (length nextRegex > 1)

algorithmStep3 :: [String] -> [String]
algorithmStep3 = map mapRegex
