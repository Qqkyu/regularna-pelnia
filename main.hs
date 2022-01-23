import System.IO
import AlgorithmStep1
import AlgorithmStep2

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

main :: IO ()
main = do
  printAllowedRegexCharacters
  r <- getLine
  print "End of program"
