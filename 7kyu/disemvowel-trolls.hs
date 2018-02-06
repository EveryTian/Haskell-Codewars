-- https://www.codewars.com/kata/disemvowel-trolls

module Disemvowel where

disemvowel :: String -> String
disemvowel = filter (not . (`elem` "aeiouAEIOU"))
