module Isogram (isIsogram) where

import Data.List (nub)
import Data.Char (toLower)

isIsogram :: String -> Bool
isIsogram str = let s = f str
                in nub s == s
    where f "" = ""
          f (' ':xs) = f xs
          f ('-':xs) = f xs
          f (x:xs) = toLower x : f xs
