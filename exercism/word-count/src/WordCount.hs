module WordCount (wordCount) where

import Data.List (sort, group)
import Data.Char (isAlpha, isDigit, toLower)
import Control.Arrow ((&&&))

wordCount :: String -> [(String, Int)]
wordCount = map (head &&& length) . group . sort . map (trim '\'') . splitBy splitFunc . map toLower
    where splitFunc c = not $ isAlpha c || isDigit c || c == '\''

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f arr = splitBy' f arr []
    where splitBy' _ [] l = [reverse l | not $ null l]
          splitBy' f (x:xs) l
              | f x = if null l then splitBy' f xs []
                      else reverse l : splitBy' f xs []
              | otherwise = splitBy' f xs (x:l)

trim :: Eq a => a -> [a] -> [a]
trim a = reverse . trimStart a . reverse . trimStart a
   where trimStart _ [] = []
         trimStart a arr@(x:xs)
             | a == x = trimStart a xs
             | otherwise = arr
