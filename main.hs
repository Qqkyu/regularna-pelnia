import System.IO
import AlgorithmStep0
import AlgorithmStep1
import AlgorithmStep2
import AlgorithmStep3
import AlgorithmStep4
import AlgorithmStep5
import AlgorithmStep6
import AlgorithmStep7

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
  putStrLn "Unnested parentheses allowed"
  putStrLn "Redundant parentheses forbidden"

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

algorithm :: String -> Bool
algorithm r =
  if algorithmStep0 r then
    algorithmStep7 $ algorithmStep6 $ algorithmStep5 $ algorithmStep4 $ algorithmStep3 $ algorithmStep2 $ algorithmStep1 r
  else
    False

main :: IO ()
main = do
  printAllowedRegexCharacters
  r <- prompt "Regular expression: "
  if algorithm r then
    putStrLn "Regular expression generates full language"
  else
    putStrLn "Regular expression does not generate full language"
