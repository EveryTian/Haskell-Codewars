-- https://www.codewars.com/kata/title-case

module TitleCase (titleCase) where

import Data.Char(toLower, toUpper)

titleCase :: String -> String -> String
titleCase "" "" = ""
titleCase minor title = let minorWords = map lower $ words minor
                            (firstWord:afterWords) = map lower $ words title
    in unwords ((newWord' firstWord):(map (newWord minorWords) afterWords))
    where a `eq` b = lower a == lower b
          ele `elem'` [] = False
          ele `elem'` (x:xs)
              | ele == x = True
              | otherwise = ele `elem'` xs
          newWord _ "" = ""
          newWord minorWords word 
              | word `elem'` minorWords = lower word
              | otherwise = newWord' word
          newWord' (x:xs) = (toUpper x):(lower xs)
          lower = map toLower
