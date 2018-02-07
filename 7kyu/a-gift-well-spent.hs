-- https://www.codewars.com/kata/a-gift-well-spent

module Gift where

buy :: (Num a, Eq a) => a -> [a] -> Maybe (Int, Int)
buy c is = buy' c is 0
    where buy' c [] _ = Nothing
          buy' c [_] _ = Nothing
          buy' c (x:xs) curIndex = if (c - x) `elem` xs
                                   then Just (curIndex, ((c - x) `index` xs) + curIndex + 1)
                                   else buy' c xs (curIndex + 1)
          n `index` arr = index' n arr 0
          index' n [] _ = -1
          index' n (x:xs) i = if n == x then i
                              else index' n xs (i + 1)
