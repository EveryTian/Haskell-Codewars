-- https://www.codewars.com/kata/palindrome-chain-length

module PalindromeChain where

palindromeChainLength :: Integer -> Integer
palindromeChainLength = f 0
    where f count n
              | show n == (reverse $ show n) = count
              | otherwise = f (count + 1) (n + (read . reverse . show $ n :: Integer))
