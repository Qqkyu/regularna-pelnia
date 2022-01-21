import System.IO

printAllowedRegexCharacters :: IO ()
printAllowedRegexCharacters = do
  putStrLn "Allowed constants:"
  putStrLn "a - literal character"
  putStrLn "e - empty string (epsilon)"
  putStrLn "Allowed operations:"
  putStrLn "(R*) - Kleene star"
  putStrLn "(RS) - concatenation"
  putStrLn "(R+S) - alternation"
  putStrLn "Unnested parentheses allowed"

contains :: String -> String -> Bool
contains str subStr
  | length str < length subStr         = False
  | length str == length subStr        = str == subStr
  | take (length subStr) str == subStr = True
  | otherwise                          = contains (tail str) subStr

doesAlgorithmStep1Pass :: String -> Bool
doesAlgorithmStep1Pass = flip contains "a*"

doesAlgorithmStep2Pass :: String -> Bool
doesAlgorithmStep2Pass r = not ((flip contains r "aa*") || (flip contains r "a*a"))

generatesFullLanguage :: String -> Bool
generatesFullLanguage r = True

main :: IO ()
main = do
  printAllowedRegexCharacters
  r <- getLine
  print (doesAlgorithmStep2Pass r)
