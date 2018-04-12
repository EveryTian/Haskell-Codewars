module Pangram (isPangram) where

import Data.List (nub)
import Data.Char (toLower)

isPangram :: String -> Bool
isPangram = (== 26) . length . nub . map toLower . filter (`elem` (['a' .. 'z'] ++ ['A' .. 'Z']))
