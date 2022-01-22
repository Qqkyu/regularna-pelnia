module AlgorithmStep2 where

-- splits regex into multiple by parentheses - (a+e)(a*e)+a-> ["a+e","a*e","+a"]
algorithmStep2 :: String -> [String]
algorithmStep2 r =
  if length r /= 0 then
    if length newRegex == 0 then
      if length rest == 0 then
        []
      else
        algorithmStep2 rest
    else
      if length rest == 0 then
        [newRegex]
      else
        newRegex : algorithmStep2 rest
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
