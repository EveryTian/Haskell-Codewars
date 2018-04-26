module Allergies (Allergen(..), allergies, isAllergicTo) where

import Data.Bits

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq)

alList = [Eggs
    , Peanuts
    , Shellfish
    , Strawberries
    , Tomatoes
    , Chocolate
    , Pollen
    , Cats]

index :: Eq a => a -> [a] -> Int
index a arr = index' a arr 0
    where index' _ [] _ = -1
          index' a (x:xs) n
              | a == x = n
              | otherwise = index' a xs (n + 1)

allergies :: Int -> [Allergen]
allergies score = [alList !! i | i <- [0 .. length alList - 1], testBit score i]

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = testBit score (index allergen alList)
