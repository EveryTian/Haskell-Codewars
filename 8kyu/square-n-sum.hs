-- https://www.codewars.com/kata/square-n-sum

module SquareSum where

squareSum :: [Integer] -> Integer
squareSum = sum . map (\x -> x * x)
