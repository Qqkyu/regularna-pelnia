import System.IO
import AlgorithmStep1
import AlgorithmStep2

printAllowedRegexCharacters :: IO ()
printAllowedRegexCharacters = do
  putStrLn "Allowed constants:"
  putStrLn "a - literal character"
  putStrLn "e - empty string (epsilon)"
  putStrLn "Allowed operations:"
  putStrLn "R* - Kleene star"
  putStrLn "RS - concatenation"
  putStrLn "R+S - alternation"
  putStrLn "Additional information:"
  putStrLn "Unnested parentheses allowed"
  putStrLn "Redundant parentheses forbidden"

generatesFullLanguage :: String -> Bool
generatesFullLanguage r = True

main :: IO ()
main = do
  printAllowedRegexCharacters
  r <- getLine
  print (doesAlgorithmStep2Pass r)
