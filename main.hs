import System.IO
import Algorithm

printAllowedRegexCharacters :: IO ()
printAllowedRegexCharacters = do
  putStrLn "-----Allowed constants-----"
  putStrLn "a - literal character"
  putStrLn "e - empty string (epsilon)"
  putStrLn "-----Allowed operations-----"
  putStrLn "R* - Kleene star"
  putStrLn "RS - concatenation"
  putStrLn "R+S - alternation"
  putStrLn "-----Additional information-----"
  putStrLn "Spaces allowed"
  putStrLn "Redundant and nested parentheses allowed"

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

main :: IO ()
main = do
  printAllowedRegexCharacters
  r <- prompt "Regular expression: "
  if algorithm r then
    putStrLn "Regular expression generates full language"
  else
    putStrLn "Regular expression does not generate full language"
