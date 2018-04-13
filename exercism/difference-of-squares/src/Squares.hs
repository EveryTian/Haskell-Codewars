module Squares (difference, squareOfSums, sumOfSquares) where

import Control.Monad (liftM2)

difference :: Integral a => a -> a
difference = liftM2 (-) squareOfSums sumOfSquares

squareOfSums :: Integral a => a -> a
squareOfSums = (^ 2) . sum . enumFromTo 1

sumOfSquares :: Integral a => a -> a
sumOfSquares = sum . map (^ 2) . enumFromTo 1
