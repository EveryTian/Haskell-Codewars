module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired xs = f xs ""
    where f "" stack = null stack
          f (']':xs) ('[':ys) = f xs ys
          f ('}':xs) ('{':ys) = f xs ys
          f (')':xs) ('(':ys) = f xs ys
          f (x:xs) stack
              | x `elem` "])}" = False
              | x `elem` "[({" = f xs (x:stack)
              | otherwise = f xs stack
