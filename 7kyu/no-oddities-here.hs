-- https://www.codewars.com/kata/no-oddities-here

module Codewars.Oddities where

noOdds :: Integral n => [n] -> [n]
noOdds = filter even
