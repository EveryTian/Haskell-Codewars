-- https://www.codewars.com/kata/least-common-multiple

module LeastCommonMultiple where
import Prelude hiding (lcm)

lcm :: Integral a => [a] -> a
lcm = foldl lcm' 1

lcm' :: Integral a => a -> a -> a
lcm' x y = let z = gcd' x y
           in if z == 0 then 0 else x * y `div` z

gcd' :: Integral a => a -> a -> a
gcd' x 0 = x
gcd' x y = gcd' y (x `mod` y)
