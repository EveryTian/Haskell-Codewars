-- https://www.codewars.com/kata/fix-it

module Fixit where
import Prelude hiding (reverse, foldr)

reverse' :: ([a] -> [a]) -> [a] -> [a]
reverse' _ [] = []
reverse' f (x:xs) = f xs ++ [x]

foldr' :: ((a -> b -> b) -> b -> [a] -> b) -> (a -> b -> b) -> b -> [a] -> b
foldr' _ _ a [] = a
foldr' f g a (x:xs) = g x (f g a xs)
