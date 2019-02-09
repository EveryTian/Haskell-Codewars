-- https://www.codewars.com/kata/counting-sheep-dot-dot-dot

module Codewars.Kata.Sheep where

countSheep :: [Bool] -> Int
countSheep = length . filter id
