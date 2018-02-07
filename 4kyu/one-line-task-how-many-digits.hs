-- https://www.codewars.com/kata/one-line-task-how-many-digits

module HaskellGolf where
f n=fromIntegral$9*sum[i*10^(i-1)|i<-[1..n]]