module AlgorithmStep2 where

getEmptyNewRegexResult :: String -> [String]
getEmptyNewRegexResult rest =
  if length rest == 0 then
    []
  else
    algorithmStep2 rest

-- splits regex into multiple by parentheses - a+(a+e)(a*e)+a*e -> ["a","+","a+e","a*e","+","a*e"]
algorithmStep2 :: String -> [String]
algorithmStep2 r =
  if length r /= 0 then
    if length newRegex == 0 then
      getEmptyNewRegexResult rest
    else
      if length rest == 0 then
        if head newRegex == '+' then
          if last newRegex == '+' then
            ["+", tail (init newRegex), "+"]
          else
            ["+", tail newRegex]
        else
          if last newRegex == '+' then
            [init newRegex, "+"]
          else
            [newRegex]
      else
        if head newRegex == '+' then
          if last newRegex == '+' then
            ["+", tail (init newRegex), "+"] ++ algorithmStep2 rest
          else
            ["+", tail newRegex] ++ algorithmStep2 rest
        else
          if last newRegex == '+' then
            [init newRegex, "+"] ++ algorithmStep2 rest
          else
            [newRegex] ++ algorithmStep2 rest
  else
    []
  where
    ongoingRegex = if length r == 0 then False else head r == '('
    newRegex =
      if length r /= 0 then
        if ongoingRegex then
          takeWhile (')'/=) (tail r)
        else
          takeWhile ('('/=) r
      else
        ""
    rest =
      if length r /= 0 then
        if ongoingRegex then
          tail (dropWhile (')'/=) (tail r))
        else
          dropWhile ('('/=) (tail r)
      else
        ""
