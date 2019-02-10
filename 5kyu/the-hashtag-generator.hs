-- https://www.codewars.com/kata/the-hashtag-generator

module Codewars.Kata.Hashtag where

import Data.Char (toUpper)

generateHashtag :: String -> Maybe String
-- generateHashtag str
--    | length str > 140 || words str == [] = Nothing
--    | otherwise = Just . ('#' :) . foldl1 (++) . map upperFstChar . words $ str
--    where upperFstChar "" = ""
--          upperFstChar (x:xs) = (toUpper x):xs
generateHashtag s =
  let ss = '#' : concatMap (\ (x:xs) -> toUpper x : xs) (words s)
  in if length ss > 140 || length ss == 1 then Nothing else Just ss
