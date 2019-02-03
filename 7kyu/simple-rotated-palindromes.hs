-- https://www.codewars.com/kata/simple-rotated-palindromes

module SimpleRotatedPalin where 

solve :: String -> Bool
solve s = palindromes s || solve' (left s) s
    where palindromes :: String -> Bool
          palindromes s = s == reverse s
          left :: String -> String
          left "" = ""
          left x = let len' = length x - 1
                   in (x !! len') : take len' x
          solve' :: String -> String -> Bool
          solve' s oriS
              | palindromes s = True
              | s == oriS = False
              | otherwise = solve' (left s) oriS
