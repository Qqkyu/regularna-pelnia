module AlgorithmStep4 where
import Utilities

findLhs :: String -> String
findLhs = takeWhile (/='+')

findRhs :: String -> String
findRhs r = takeWhile (/='+') (tail (dropWhile (/='+') r))

findRest :: String -> String
findRest r = dropWhile (/='+') (tail (dropWhile (/='+') r))

handleOtherLanguageEpsilon :: String -> Char -> String
handleOtherLanguageEpsilon r rLanguage =
  if rLanguage == 'M' then
    if findNumber r == "0" then
      "X"
    else
      "N[1." ++ (findNumber r) ++ "]"
  else
    if rLanguage == 'P' then
      "P[0," ++ (findNumber r) ++ "]"
    else
      if rLanguage == '0' then
        "0"
      else
        "X"

handleOneLanguageEpsilon :: String -> String -> Char -> Char -> String
handleOneLanguageEpsilon lhs rhs lhsLanguage rhsLanguage =
  if lhsLanguage == '0' then
    handleOtherLanguageEpsilon rhs rhsLanguage
  else
    handleOtherLanguageEpsilon lhs lhsLanguage

handleOtherLanguageM :: String -> String -> Char -> String
handleOtherLanguageM other r rLanguage =
  if rLanguage == 'M' then
    if findNumberInt other < findNumberInt r then
      "M[" ++ (findNumber other) ++ "]"
    else
      "M[" ++ (findNumber r) ++ "]"
  else
    if rLanguage == 'P' then
      if findNumberInt other <= findNumberInt r then
        "M[" ++ (show ((findNumberInt r) - (findNumberInt other))) ++ "]"
      else
        "N[0." ++ (show ((findNumberInt r) - 1)) ++ "," ++ (show ((findNumberInt r) + 1)) ++ "." ++ (findNumber other) ++ "]"
    else
      "X"

handleOneLanguageM :: String -> String -> Char -> Char -> String
handleOneLanguageM lhs rhs lhsLanguage rhsLanguage =
  if lhsLanguage == 'M' then
    handleOtherLanguageM lhs rhs rhsLanguage
  else
    handleOtherLanguageM rhs lhs lhsLanguage

handleOtherLanguageP :: String -> String -> Char -> String
handleOtherLanguageP other r rLanguage =
  if rLanguage == 'P' then
    if findNumberInt r < findNumberInt other then
      "P[" ++ (findNumber r) ++ "," ++ (findNumber other) ++ "]"
    else
      "P[" ++ (findNumber other) ++ "," ++ (findNumber r) ++ "]"
  else
    "X"

handleOneLanguageP :: String -> String -> Char -> Char -> String
handleOneLanguageP lhs rhs lhsLanguage rhsLanguage =
  if lhsLanguage == 'P' then
    handleOtherLanguageP lhs rhs rhsLanguage
  else
    handleOtherLanguageP rhs lhs lhsLanguage

transformAlternation :: String -> String -> String
transformAlternation lhs rhs =
  if lhsLanguage == '0' || rhsLanguage == '0' then
    handleOneLanguageEpsilon lhs rhs lhsLanguage rhsLanguage
  else
    if lhsLanguage == 'M' || rhsLanguage == 'M' then
      handleOneLanguageM lhs rhs lhsLanguage rhsLanguage
    else
      if lhsLanguage == 'P' || rhsLanguage == 'P' then
        handleOneLanguageP lhs rhs lhsLanguage rhsLanguage
      else
        "X"
  where
    lhsLanguage = head lhs
    rhsLanguage = head rhs

transformFirstAlternation :: String -> String
transformFirstAlternation r =
  (transformAlternation lhs rhs) ++ rest
  where
    lhs = findLhs r
    rhs = findRhs r
    rest = findRest r

transformRegexIfNeeded :: String -> String
transformRegexIfNeeded r =
  if contains r "+" && r /= "+" then
    transformRegexIfNeeded $ transformFirstAlternation r
  else
    r

algorithmStep4 :: [String] -> [String]
algorithmStep4 = map transformRegexIfNeeded
