module Isogram (isIsogram) where

import Data.List (nub)
import Data.Char (toLower)

isIsogram :: String -> Bool
isIsogram str =
    let s = map toLower . filter (`notElem` " -") $ str
    in nub s == s
