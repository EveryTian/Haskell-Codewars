-- https://www.codewars.com/kata/from-to-step-sequence-generator

module FromToStepGenerator where

generator :: Integer -> Integer -> Integer -> [Integer]
generator from to step
    | step == 0 = []
    | from < to = [from, from + abs step .. to]
    | otherwise = [from, from - abs step .. to]
        where abs x = if x >= 0 then x else -x
