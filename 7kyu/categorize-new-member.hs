-- https://www.codewars.com/kata/categorize-new-member

module Codewars.Kata.Categorize where
import Codewars.Kata.Categorize.Types

-- data Membership = Open | Senior deriving (Eq, Show)
openOrSenior :: [(Int, Int)] -> [Membership]
openOrSenior = map (\x -> if fst x >= 55 && snd x > 7 then Senior else Open)
