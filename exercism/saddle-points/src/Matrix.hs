module Matrix (saddlePoints) where

import Data.Array (Array, assocs)

saddlePoints :: Ord a => Array (Int, Int) a -> [(Int, Int)]
saddlePoints mx = let rowsNum = getRowsNum mx
                  in [(r, c) | r <- [0 .. rowsNum - 1],
                               (c, n) <- getMaxsWithCol $ getRow r mx,
                               n == minimum (getCol c mx)]
    where getMaxsWithCol :: Ord a => [a] -> [(Int, a)]
          getMaxsWithCol = f 0 []
              where f :: Ord a => Int -> [(Int, a)] -> [a] -> [(Int, a)]
                    f _ result [] = result
                    f c [] (x:xs) = f (c + 1) [(c, x)] xs
                    f c result (x:xs)
                        | x == snd (head result) = f (c + 1) ((c, x):result) xs
                        | x > snd (head result) = f (c + 1) [(c, x)] xs
                        | otherwise = f (c + 1) result xs
          getRow :: Int -> Array (Int, Int) a -> [a]
          getRow r = map snd . filter ((==r) . fst . fst) . assocs
          getCol :: Int -> Array (Int, Int) a -> [a]
          getCol c = map snd . filter ((==c) . snd . fst) . assocs
          getRowsNum :: Array (Int, Int) a -> Int
          getRowsNum mx = test 0
              where test n = case getRow n mx of
                        [] -> n
                        _ -> test $ n + 1
