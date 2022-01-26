import System.IO
import AlgorithmStep1
import AlgorithmStep2
import AlgorithmStep3
import AlgorithmStep4
import AlgorithmStep5
import AlgorithmStep6
import AlgorithmStep7

removeSpaces :: String -> String
removeSpaces [] = []
removeSpaces (a : as)
  | a == ' '  = removeSpaces as
  | otherwise = a : removeSpaces as

printAllowedRegexCharacters :: IO ()
printAllowedRegexCharacters = do
  putStrLn "---Allowed constants---"
  putStrLn "a - literal character"
  putStrLn "e - empty string (epsilon)"
  putStrLn "---Allowed operations---"
  putStrLn "R* - Kleene star"
  putStrLn "RS - concatenation"
  putStrLn "R+S - alternation"
  putStrLn "---Additional information---"
  putStrLn "Spaces allowed"
  putStrLn "Unnested parentheses allowed"
  putStrLn "Redundant parentheses forbidden"
  putStrLn "Max one alternation in one part (a+a+a disallowed)"

generatesFullLanguage :: String -> Bool
generatesFullLanguage r = True

algorithm :: String -> Bool
algorithm r =
  if algorithmStep1 r then
    algorithmStep7 $ algorithmStep6 $ algorithmStep5 $ algorithmStep4 $ algorithmStep3 $ algorithmStep2 r
  else
    False

main :: IO ()
main = do
  -- printAllowedRegexCharacters
  r <- getLine
  if algorithm r then
    print "Regular expression generates full language"
  else
    print "Regular expression does not generate full language"
