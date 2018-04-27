module Matrix
    ( Matrix
    , cols
    , column
    , flatten
    , Matrix.fromList
    , fromString
    , reshape
    , row
    , rows
    , shape
    , transpose
    ) where

import Data.Vector (Vector)
import Data.Vector as V

data Matrix a = Matrix {
    flatten :: Vector a,
    rows :: Int,
    cols :: Int 
} deriving (Eq, Show)

column :: Int -> Matrix a -> Vector a
column x (Matrix m r c) = V.map (\ i -> m ! (i * c + x)) $ V.enumFromTo 0 (r - 1)

fromList :: [[a]] -> Matrix a
fromList [] = Matrix V.empty 0 0
fromList (x:xs) = case Matrix.fromList xs of
    Matrix m r c -> Matrix (V.fromList x V.++ m) (r + 1) (Prelude.length x)

fromString :: Read a => String -> Matrix a
fromString = Matrix.fromList . Prelude.map (Prelude.map read . words) . lines

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (r, c) (Matrix m _ _) = Matrix m r c

row :: Int -> Matrix a -> Vector a
row x (Matrix m _ c) = V.take c $ V.drop (x * c) m

shape :: Matrix a -> (Int, Int)
shape (Matrix _ r c) = (r, c)

transpose :: Matrix a -> Matrix a
transpose mx@(Matrix m r c) = Matrix m' c r
    where m' = V.concatMap (`column` mx) $ V.enumFromTo 0 (c - 1)
