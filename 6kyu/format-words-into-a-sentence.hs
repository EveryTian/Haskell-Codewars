-- https://www.codewars.com/kata/format-words-into-a-sentence

module FormatSentence where 

import Data.List (intercalate)

formatWords :: [String] -> String 
formatWords xs = case filter (not . null) xs of
    [] -> ""
    [x] -> x
    nxs -> intercalate ", " (init nxs) ++ " and " ++ last nxs
