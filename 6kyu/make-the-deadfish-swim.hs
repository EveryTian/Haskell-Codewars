-- https://www.codewars.com/kata/make-the-deadfish-swim

module Kata.Deadfish (parse) where

parse :: String -> [Int]
parse = f 0
    where f _ [] = []
          f n ('i':s) = f (n + 1) s
          f n ('d':s) = f (n - 1) s
          f n ('s':s) = f (n * n) s
          f n ('o':s) = n:f n s
          f n (_:s) = f n s
