module AlgorithmStep7 where
import Utilities

convertToInts :: String -> [Int]
convertToInts "" = []
convertToInts list = (read curNum :: Int) : convertToInts rest
  where
    curNum = takeWhile(\x -> x /= ',') list
    rest = 
      if length curNum == length list then 
        ""
      else
        tail (dropWhile(\x -> x /= ',') list)

createNewSmallestMAcc :: Int -> String -> Int
createNewSmallestMAcc acc lang =
  if langType == 'M' && (findNumberInt lang) < acc then
    findNumberInt lang
  else
    acc
  where
    langType = head lang

findSmallestM :: [String] -> Int
findSmallestM = foldl createNewSmallestMAcc maxBound

countLiterals :: [String] -> [Int]
countLiterals [] = []
countLiterals (lang : langs) =
  if langType == '0' then
    [0] ++ countLiterals langs
  else
    if langType == 'P' then
      (convertToInts $ findNumber lang) ++ countLiterals langs
    else
      countLiterals langs
  where
    langType = head lang

createList :: [String] -> ([Int], Int)
createList langs = (countLiterals langs, findSmallestM langs)

-- list has to have elements from range [0,ceil] to return True
checkListResult :: ([Int], Int) -> Bool
checkListResult (list,ceil) = foldl (\acc x -> if x `elem` list then acc else False) True [0..ceil]

processLangsList :: [String] -> Bool
processLangsList langs =
  checkListResult $ list
  where
    list = createList langs

algorithmStep7 :: [String] -> Bool
algorithmStep7 langs =
  if "X" `elem` langs then
    True
  else
    processLangsList langs
