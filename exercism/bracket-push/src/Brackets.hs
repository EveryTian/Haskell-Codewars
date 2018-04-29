module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired xs = f xs ""
    where f "" stack = null stack
          f (']':xs) stack = not (null stack) && head stack == '[' && f xs (tail stack)
          f ('}':xs) stack = not (null stack) && head stack == '{' && f xs (tail stack)
          f (')':xs) stack = not (null stack) && head stack == '(' && f xs (tail stack)
          f (x:xs) stack
              | x `elem` "[({" = f xs (x:stack)
              | otherwise = f xs stack
