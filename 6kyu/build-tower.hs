-- https://www.codewars.com/kata/build-tower

module Codewars.BuildTower where

buildTower :: Int -> [String]
buildTower n = [let spaces = take (n-i) $ repeat ' ' in spaces ++ (take (i*2-1) $ repeat '*') ++ spaces | i <- [1..n]]