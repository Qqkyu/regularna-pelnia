module AlgorithmStep6 where
import Utilities

splitLang :: String -> [String]
splitLang lang = [lhs] ++ ["+"] ++ [rhs]
  where
    lhs = tail (findLhs lang)
    rhs = init (findRhs lang)

splitLangIfNeeded :: [String] -> String -> [String]
splitLangIfNeeded acc lang =
  if contains lang "+" && length lang > 1 then
    acc ++ splitLang lang
  else
    acc ++ [lang]

algorithmStep6 :: [String] -> [String]
algorithmStep6 langs = foldl splitLangIfNeeded [] langs
