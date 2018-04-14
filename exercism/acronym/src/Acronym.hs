module Acronym (abbreviate) where

import Data.Char (toLower, toUpper, isLower, isUpper)

abbreviate :: String -> String
abbreviate = map (toUpper . head) . splitString . map toLower . preDeal
    where preDeal :: String -> String
          preDeal "" = ""
          preDeal [x] = [x]
          preDeal s@[x, y]
              | isLower x && isUpper y = [x, ' ', y]
              | otherwise = s
          preDeal (x0:x1:xs)
              | isLower x0 && isUpper x1 = x0:' ':x1:preDeal xs
              | otherwise = x0:x1:preDeal xs
          splitString :: String -> [String]
          splitString "" = []
          splitString str = filter (/= "") $ splitString' str ""
              where splitString' "" word = [reverse word]
                    splitString' (x:xs) word
                        | isLower x = splitString' xs (x:word)
                        | otherwise = reverse word : splitString xs
