-- https://www.codewars.com/kata/multiples-of-3-or-5

module MultiplesOf3And5 where

solution :: Integer -> Integer
solution number = f 5 + f 3 - f 15
    where f x = sum [i | i <- [1..number - 1], i `mod` x == 0]
