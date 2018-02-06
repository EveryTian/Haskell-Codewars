-- https://www.codewars.com/kata/two-joggers

module Joggers where

nbrOfLaps :: Integer -> Integer -> (Integer, Integer)
nbrOfLaps bob charles = 
    let gcd' = gcd bob charles in (charles `div` gcd', bob `div` gcd')
        where gcd x 0 = x
              gcd x y = gcd y (x `mod` y)
