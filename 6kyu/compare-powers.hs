-- https://www.codewars.com/kata/compare-powers

module Codewars.Exercise.Powers where
import Data.Word
type Power = (Word, Word)

comparePowers :: Power -> Power -> Ordering
comparePowers (0, _) (0, _) = EQ
comparePowers (1, _) (1, _) = EQ
comparePowers (n1, p1) (n2, p2)
    | n1 == n2 && p1 == p2 = EQ
    | n1 <= n2 && p1 <= p2 = LT
    | n1 >= n2 && p1 >= p2 = GT
    | otherwise = let n1' = fromIntegral n1
                      n2' = fromIntegral n2
                      p1' = fromIntegral p1
                      p2' = fromIntegral p2
                  in compare (log n1' * p1') (log n2' * p2')
