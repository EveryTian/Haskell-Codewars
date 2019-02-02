-- https://www.codewars.com/kata/challenge-fun-number-14-target-game

module Kata.TargetGame (targetGame) where

targetGame :: [Int] -> Int
targetGame = head . (flip f) []

f :: [Int] -> [Int] -> [Int]
f [] [] = [0]
f [] rss = rss
f (x:xs) [] = f xs [max x 0]
f (x:xs) [r] = f xs [max x r, r]
f (x:xs) rss@(r0:r1:rs) = f xs ((max (x + r1) r0):rss)
