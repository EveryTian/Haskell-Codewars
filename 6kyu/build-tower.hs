-- https://www.codewars.com/kata/build-tower

module Codewars.BuildTower where

buildTower :: Int -> [String]
buildTower n = [let spaces = replicate (n - i) ' ' in spaces ++ replicate (i * 2 - 1) '*' ++ spaces | i <- [1..n]]
