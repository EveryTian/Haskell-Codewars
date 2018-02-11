-- https://www.codewars.com/kata/pete-the-baker

module Baker where

type Ingredient = String
type Amount     = Int
type Recipe     = [(Ingredient, Amount)]
type Storage    = [(Ingredient, Amount)]

cakes :: Recipe -> Storage -> Int
cakes recipe storage = minimum [getMax i storage | i <- recipe]

getMax :: (Ingredient, Amount) -> Storage -> Int
getMax oneRecipe [] = 0
getMax oneRecipe (x:xs)
    | fst oneRecipe == fst x = snd x `div` snd oneRecipe
    | otherwise = getMax oneRecipe xs
