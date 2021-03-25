module Minesweeper (annotate) where

annotate :: [String] -> [String]
annotate board = merge board $ i2s $ shiftSum $ s2i board

shiftSum :: [[Int]] -> [[Int]]
shiftSum x = foldl1 (+++) xList
  where xList = [f x | f <- [leftShift, rightShift, upShift, downShift, leftUpShift, leftDownShift, rightUpShift, rightDownShift]]

(+++) :: [[Int]] -> [[Int]] -> [[Int]]
[] +++ _ = []
_ +++ [] = []
(arrx:arrxs) +++ (arry:arrys) = zipWith (+) arrx arry : (arrxs +++ arrys)

merge :: [String] -> [String] -> [String]
merge [] _ = []
merge _ [] = []
merge (o:oriBoard) (g:genBoard) = mergeLine o g : merge oriBoard genBoard
  where mergeLine :: String -> String -> String
        mergeLine [] _ = []
        mergeLine _ [] = []
        mergeLine ('*':oris) (_:gens) = '*' : mergeLine oris gens
        mergeLine (_:oris) (gen:gens) = gen : mergeLine oris gens

s2i :: [String] -> [[Int]]
s2i = map $ map c2i
  where c2i :: Char -> Int
        c2i ' ' = 0
        c2i '*' = 1
        c2i _ = 0

i2s :: [[Int]] -> [String]
i2s = map $ map i2c
  where i2c :: Int -> Char
        i2c 0 = ' '
        i2c 1 = '1'
        i2c 2 = '2'
        i2c 3 = '3'
        i2c 4 = '4'
        i2c 5 = '5'
        i2c 6 = '6'
        i2c 7 = '7'
        i2c 8 = '8'
        i2c _ = ' '
              
leftShift :: [[Int]] -> [[Int]]
leftShift = map leftShiftLine

leftShiftLine :: [Int] -> [Int]
leftShiftLine [] = []
leftShiftLine [_] = [0]
leftShiftLine (_:xs) = xs ++ [0]

rightShift :: [[Int]] -> [[Int]]
rightShift = map rightShiftLine

rightShiftLine :: [Int] -> [Int]
rightShiftLine [] = []
rightShiftLine [_] = [0]
rightShiftLine arr = 0 : take (length arr - 1) arr

upShift :: [[Int]] -> [[Int]]
upShift [] = []
upShift [[]] = [[]]
upShift [arr] = [replicate (length arr) 0]
upShift (arr:arrs) = arrs ++ [replicate (length arr) 0]

downShift :: [[Int]] -> [[Int]]
downShift [] = []
downShift [[]] = [[]]
downShift [arr] = [replicate (length arr) 0]
downShift board@(arr:_) = replicate (length arr) 0 : take (length board - 1) board

leftUpShift :: [[Int]] -> [[Int]]
leftUpShift = leftShift . upShift

leftDownShift :: [[Int]] -> [[Int]]
leftDownShift = leftShift . downShift

rightUpShift :: [[Int]] -> [[Int]]
rightUpShift = rightShift . upShift

rightDownShift :: [[Int]] -> [[Int]]
rightDownShift = rightShift . downShift

