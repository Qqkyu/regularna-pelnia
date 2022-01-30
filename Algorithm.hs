module Algorithm where
import AlgorithmStep0
import AlgorithmStep1
import AlgorithmStep2
import AlgorithmStep3
import AlgorithmStep4
import AlgorithmStep5
import AlgorithmStep6
import AlgorithmStep7
import Utilities

performAlgorithmSteps :: String -> [String]
performAlgorithmSteps r = algorithmStep6 $ algorithmStep5 $ algorithmStep4 $ algorithmStep3 $ algorithmStep2 $ algorithmStep1 r

createSingleP :: Int -> String
createSingleP x =
  if x == 0 then
    "e"
  else
    duplicate "a" x

parseP :: String -> String
parseP r = 
  if length list /= 0 then
    tail (foldl (\acc x -> acc ++ "+" ++ (createSingleP x)) "" list)
  else
    ""
  where
    list = convertToInts $ findNumber r

mapSingleAlgPart :: String -> String
mapSingleAlgPart x =
  if x == "+" then
    "+"
  else
    if x == "X" then
      "a*"
    else
      if x == "0" then
        "e"
      else
        if head x == 'M' then
          (duplicate "a" ((findNumberInt x) + 1)) ++ "a*"
        else
          parseP x

parseAlgorithmResult :: [String] -> String
parseAlgorithmResult r =
  foldl (\acc x -> acc ++ (mapSingleAlgPart x)) "" r

handleParensClose :: Int -> String -> String
handleParensClose 1 curRegex = parseAlgorithmResult $ performAlgorithmSteps $ curRegex
handleParensClose openParens curRegex =
    if head parensRegex == '(' then
      parseAlgorithmResult $ performAlgorithmSteps (outermostRegex ++ (handleParensClose (openParens - 1) (tail parensRegex)))
    else
      parseAlgorithmResult $ performAlgorithmSteps (outermostRegex ++ (handleParensClose (openParens - 1) parensRegex))
  where
    outermostRegex = takeWhile ('('/=) curRegex
    parensRegex = takeWhile (')'/=) (dropWhile ('('/=) curRegex)

regexParser :: Int -> String -> String -> String
regexParser 0 curRegex [] = parseAlgorithmResult $ performAlgorithmSteps curRegex
regexParser x curRegex [] = error "Incorrect regular expression!"
regexParser openParens curRegex (c : cs) =
  if openParens == 0 then
    if c == '(' then
      (parseAlgorithmResult $ performAlgorithmSteps curRegex) ++ "(" ++ regexParser 1 "" cs
    else
      regexParser 0 (curRegex ++ [c]) cs
  else
    if c == '(' then
      regexParser (openParens + 1) (curRegex ++ [c]) cs
    else
      if c == ')' then
        (parseAlgorithmResult $ performAlgorithmSteps $ (curRegex ++ ")")) ++ ")" ++ regexParser 0 "" cs
      else
        regexParser openParens (curRegex ++ [c]) cs

algorithm :: String -> Bool
algorithm r = algorithmStep7 $ performAlgorithmSteps (regexParser 0 "" r)

