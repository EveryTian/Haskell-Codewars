-- https://www.codewars.com/kata/pick-peaks

module PickPeak.JorgeVS.Kata where

import Data.List (groupBy)
import Data.Function (on)

data PickedPeaks = PickedPeaks { pos :: [Int], peaks :: [Int]} deriving (Eq, Show)

pickPeaks :: [Int] -> PickedPeaks
pickPeaks xs = pickPeaks' (head xs) (preDeal xs) (PickedPeaks [] [])
    where preDeal :: [Int] -> [(Int, Int)]
          preDeal = preDeal2 . preDeal1 0
              where preDeal1 :: Int -> [Int] -> [(Int, Int)]
                    preDeal1 _ [] = []
                    preDeal1 curPos (x:xs) = (x, curPos) : preDeal1 (curPos + 1) xs
                    preDeal2 :: [(Int, Int)] -> [(Int, Int)]
                    preDeal2 = map (!! 0) . groupBy ((==) `on` fst)
          pickPeaks' :: Int -> [(Int, Int)] -> PickedPeaks -> PickedPeaks
          pickPeaks' _ [] pickedPeaks = pickedPeaks
          pickPeaks' _ [_] pickedPeaks = pickedPeaks
          pickPeaks' pre ((x0, pos0):(x1, pos1):xs) pickedPeaks@(PickedPeaks pos peaks)
              | pre < x0 && x0 >= x1 = pickPeaks' x0 ((x1, pos1):xs) (PickedPeaks (pos ++ [pos0]) (peaks ++ [x0]))
              | otherwise = pickPeaks' x0 ((x1, pos1):xs) pickedPeaks
