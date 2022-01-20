import System.IO

data SetOfStates = SetOfStates [String]
data SetOfInputSymbols = SetOfInputSymbols [String]
data TransitionFunction = TransitionFunction [(String,String,String)]
data StartState = StartState String
data SetOfAcceptStates = SetOfAcceptStates [String]
data DFA = DFA SetOfStates SetOfInputSymbols TransitionFunction StartState SetOfAcceptStates

languageMinimalDFA :: DFA
languageMinimalDFA = DFA (SetOfStates ["S"]) (SetOfInputSymbols ["a"]) (TransitionFunction [("S","a","S")]) (StartState "S") (SetOfAcceptStates [("S")])

printAllowedRegexCharacters :: IO ()
printAllowedRegexCharacters = do
  putStrLn "Allowed constants:"
  putStrLn "a - literal character"
  putStrLn "Allowed operations:"
  putStrLn "(R*) - Kleene star"
  putStrLn "(RS) - concatenation"
  putStrLn "(R|S) - alternation"

main :: IO ()
main = do
  printAllowedRegexCharacters
