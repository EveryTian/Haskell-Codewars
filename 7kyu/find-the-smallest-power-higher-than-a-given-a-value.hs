-- https://www.codewars.com/kata/find-the-smallest-power-higher-than-a-given-a-value

module Kata.SmallestPerfectPowerHigherThatAGivenValue where

findNextPower :: Integer -> Integer -> Integer
findNextPower bound exp = (floor (fromIntegral bound ** (1 / fromIntegral exp)) + 1) ^ exp
