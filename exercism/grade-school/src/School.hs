module School (School, add, empty, grade, sorted) where

import Data.Maybe (fromMaybe)
import Data.List (sortBy, sort)
import Data.Function (on)

type School = [(Int, [String])]

add :: Int -> String -> School -> School
add gradeNum student school = case lookup' gradeNum school of
    Nothing -> (gradeNum, [student]) : school
    Just students -> (gradeNum, student:students) : drop' gradeNum school

empty :: School
empty = []

grade :: Int -> School -> [String]
grade n s = fromMaybe [] $ lookup' n s

sorted :: School -> [(Int, [String])]
sorted = sortBy (compare `on` fst) . sortStudents
    where sortStudents [] = []
          sortStudents ((x0, x1):xs) = (x0, sort x1) : sortStudents xs

lookup' :: Int -> School -> Maybe [String]
lookup' _ [] = Nothing
lookup' n ((x0, x1):xs)
    | n == x0 = Just $ sort x1
    | otherwise = lookup' n xs

drop' :: Int -> School -> School
drop' _ [] = []
drop' n (x:xs)
    | n == fst x = xs
    | otherwise = x : drop' n xs
