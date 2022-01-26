module AlgorithmStep5 where
import Utilities

isLanguage :: String -> Bool
isLanguage = ("+" /=)

isSecondTypeParens :: String -> Bool
isSecondTypeParens s = contains s "P"

handleOtherLanguageEpsilon :: String -> Char -> String
handleOtherLanguageEpsilon r rLanguage = r

handleOneLanguageEpsilon :: String -> String -> Char -> Char -> String
handleOneLanguageEpsilon lhs rhs lhsLanguage rhsLanguage =
  if lhsLanguage == '0' && length lhs == 1 then
    handleOtherLanguageEpsilon rhs rhsLanguage
  else
    handleOtherLanguageEpsilon lhs lhsLanguage

handleMAndSecondTypeParens :: String -> String -> String
handleMAndSecondTypeParens parens other =
  if parensLhsLanguage == 'M' then
    if lhsNum <= rhsNum then
      "M[" ++ (show (otherNum + rhsNum)) ++ "]"
    else
      "M[" ++ (show (otherNum + lhsNum + 1)) ++ "]"
  else
    if lhsNum > rhsNum then
      "M[" ++ (show (otherNum + lhsNum)) ++ "]"
    else
      "M[" ++ (show (otherNum + rhsNum + 1)) ++ "]"
  where
    parensLhs = (tail (findLhs parens))
    parensLhsLanguage = head parensLhs
    parensRhs = (init (findRhs parens))
    lhsNum = findNumberInt parensLhs
    rhsNum = findNumberInt parensRhs
    otherNum = findNumberInt other

handleOtherLanguageM :: String -> String -> Char -> String
handleOtherLanguageM other r rLanguage =
  if rLanguage == 'M' then
    "M[" ++ (show ((findNumberInt r) + (findNumberInt other) + 1)) ++ "]"
  else
    if rLanguage == 'P' then
      "M[" ++ (show ((read (findSmallestFromList r) :: Int) + (findNumberInt other))) ++ "]"
    else
      if rLanguage == 'X' then
        "M[" ++ (findNumber other) ++ "]"
      else
        if isSecondTypeParens r then
          handleMAndSecondTypeParens r other
        else  
          other

handleOneLanguageM :: String -> String -> Char -> Char -> String
handleOneLanguageM lhs rhs lhsLanguage rhsLanguage =
  if lhsLanguage == 'M' then
    handleOtherLanguageM lhs rhs rhsLanguage
  else
    handleOtherLanguageM rhs lhs lhsLanguage

concatPAndFirstParens :: String -> String -> String
concatPAndFirstParens pLang parens =
  if parensLhs == "0" then
    pLang ++ "+M[" ++ (show (findNumberInt (parensRhs) + smallestNumFromPInt)) ++ "]"
  else
    pLang ++ "+M[" ++ (show (findNumberInt (parensLhs) + smallestNumFromPInt)) ++ "]"
  where
    parensLhs = (tail (findLhs parens))
    parensRhs = (init (findRhs parens))
    smallestNumFromPInt = (read (findSmallestFromList pLang) :: Int)

handlePAndSecondTypeParens :: String -> String -> String
handlePAndSecondTypeParens parens other =
  (concatenateLanguages other parensLhs) ++ "+" ++ (concatenateLanguages other  parensRhs)
  where
    parensLhs = (tail (findLhs parens))
    parensRhs = (init (findRhs parens))

handleOtherLanguageP :: String -> String -> Char -> String
handleOtherLanguageP other r rLanguage =
  if rLanguage == 'P' then
    "P[" ++ (addLists r other) ++ "]"
  else
    if rLanguage == 'X' then
      if findSmallestFromList other == "0" then
        "X"
      else
        "M[" ++ (show ((read (findSmallestFromList other) :: Int) - 1)) ++ "]"
    else
      if isSecondTypeParens r then
        handlePAndSecondTypeParens r other
      else
        concatPAndFirstParens other r

handleOneLanguageP :: String -> String -> Char -> Char -> String
handleOneLanguageP lhs rhs lhsLanguage rhsLanguage =
  if lhsLanguage == 'P' then
    handleOtherLanguageP lhs rhs rhsLanguage
  else
    handleOtherLanguageP rhs lhs lhsLanguage

handleXAndSecondTypeParens :: String -> String
handleXAndSecondTypeParens parens =
  (concatenateLanguages "X" parensLhs) ++ "+" ++ (concatenateLanguages "X"  parensRhs)
  where
    parensLhs = (tail (findLhs parens))
    parensRhs = (init (findRhs parens))

concatenateLanguages :: String -> String -> String
concatenateLanguages lhs rhs =
  if (lhsLanguage == '0' && length lhs == 1) || (rhsLanguage == '0' && length rhs == 1) then
    handleOneLanguageEpsilon lhs rhs lhsLanguage rhsLanguage
  else
    if lhsLanguage == 'M' || rhsLanguage == 'M' then
      handleOneLanguageM lhs rhs lhsLanguage rhsLanguage
    else
      if lhsLanguage == 'P' || rhsLanguage == 'P' then
        handleOneLanguageP lhs rhs lhsLanguage rhsLanguage
      else
        if isSecondTypeParens lhs || isSecondTypeParens rhs then
          if isSecondTypeParens lhs then
            handleXAndSecondTypeParens lhs
          else
            handleXAndSecondTypeParens rhs
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
