-- https://www.codewars.com/kata/paginationhelper

module Codewars.Kata.Pagination where

type Collection a = [a]
type ItemsPerPage = Int

itemCount :: Collection a -> Int
itemCount = length

pageCount :: Collection a -> ItemsPerPage -> Int
pageCount xs n = ceiling ((fromIntegral . itemCount $ xs) / fromIntegral n)

pageItemCount :: Collection a -> ItemsPerPage -> Int -> Maybe Int
pageItemCount xs n page
    | page < 0 || page >= pageCount xs n = Nothing
    | page == pageCount xs n - 1 = Just (let x = itemCount xs `mod` n in if x == 0 then n else x)
    | otherwise = Just n

pageIndex :: Collection a -> ItemsPerPage -> Int -> Maybe Int
pageIndex [] _ _ = Nothing
pageIndex xs n item
    | item < 0 = Just 0
    | item >= itemCount xs = Nothing
    | otherwise = Just (item `div` n)
