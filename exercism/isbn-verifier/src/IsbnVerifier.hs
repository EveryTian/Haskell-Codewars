module IsbnVerifier (isbn) where

import Data.Char (ord)

isbn :: String -> Bool
isbn str = 
    let tmp = filter (`elem` 'X':['0'..'9']) str
        f c = if c == 'X' then 10 else ord c - ord '0'
        number = sum . zipWith (*) [10, 9 .. 1] $ map f tmp
    in length tmp == 10 && number `mod` 11 == 0 && 'X' `notElem` init tmp
