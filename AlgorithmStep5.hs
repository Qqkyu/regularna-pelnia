import Utilities

isLanguage :: String -> Bool
isLanguage = ("+" /=)

handleOtherLanguageEpsilon :: String -> Char -> String
handleOtherLanguageEpsilon r rLanguage =
  if rLanguage == 'M' then
    "M[" ++ (findNumber r) ++ "]"
  else
    if rLanguage == 'P' then
      "P[" ++ (findNumber r) ++ "]"
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
    "M[" ++ (show ((findNumberInt r) + (findNumberInt other) + 1)) ++ "]"
  else
    if rLanguage == 'P' then
      "M[" ++ (show ((read (findSmallestFromList r) :: Int) + (findNumberInt other))) ++ "]"
    else
      "M[" ++ (findNumber other) ++ "]"

handleOneLanguageM :: String -> String -> Char -> Char -> String
handleOneLanguageM lhs rhs lhsLanguage rhsLanguage =
  if lhsLanguage == 'M' then
    handleOtherLanguageM lhs rhs rhsLanguage
  else
    handleOtherLanguageM rhs lhs lhsLanguage

handleOtherLanguageP :: String -> String -> Char -> String
handleOtherLanguageP other r rLanguage =
  if rLanguage == 'P' then
    "P[" ++ (addLists r other) ++ "]"
  else
    if findSmallestFromList other == "0" then
      "X"
    else
      "M[" ++ (show ((read (findSmallestFromList other) :: Int) - 1)) ++ "]"

handleOneLanguageP :: String -> String -> Char -> Char -> String
handleOneLanguageP lhs rhs lhsLanguage rhsLanguage =
  if lhsLanguage == 'P' then
    handleOtherLanguageP lhs rhs rhsLanguage
  else
    handleOtherLanguageP rhs lhs lhsLanguage

concatenateLanguages :: String -> String -> String
concatenateLanguages lhs rhs =
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

removeConcatenation :: String -> [String] -> [String]
removeConcatenation prev [] = [prev]
removeConcatenation prev (cur : lang) =
  if (isLanguage prev) && (isLanguage cur) then
    removeConcatenation (concatenateLanguages prev cur) lang
  else
    prev : (removeConcatenation cur lang)

algorithmStep5 :: [String] -> [String]
algorithmStep5 langs
  | length langs < 2 = langs
  | otherwise        = removeConcatenation (head langs) (tail langs)
