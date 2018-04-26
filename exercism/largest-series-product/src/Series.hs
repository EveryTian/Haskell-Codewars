module Series (Error(..), largestProduct) where

import Data.Char (digitToInt)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct 0 _ = Right 1
largestProduct size digits = case checkError size digits of
    Just e -> Left e
    Nothing -> Right $ maximum . map product . takeElements size . map (fromIntegral . digitToInt) $ digits

checkError :: Int -> String -> Maybe Error
checkError = checkAt 0
    where checkAt index size []
              | index < size || size < 0 = Just InvalidSpan 
              | otherwise = Nothing
          checkAt index size (x:xs)
              | x `notElem` ['0'..'9'] = Just $ InvalidDigit x
              | otherwise = checkAt (index + 1) size xs

takeElements :: Int -> [a] -> [[a]]
takeElements n arr = takeElements' n arr (length arr)
    where takeElements' n a@(_:xs) arrLength
              | n > arrLength = []
              | otherwise = take n a : takeElements' n xs (arrLength - 1)
          takeElements' _ [] _ = []
