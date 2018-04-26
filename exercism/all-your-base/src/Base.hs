module Base (Error(..), rebase) where

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits = 
    case checkBase inputBase outputBase inputDigits of
        Just error -> Left error
        Nothing -> Right $ fromNum outputBase $ toNum inputBase inputDigits

checkBase :: Integral a => a -> a -> [a] -> Maybe (Error a)
checkBase ib io ds
    | ib <= 1 = Just InvalidInputBase
    | io <= 1 = Just InvalidOutputBase
    | otherwise = checkBase' ib ds 
    where checkBase' _ [] = Nothing
          checkBase' b (x:xs)
              | x < 0 || x >= b = Just $ InvalidDigit x
              | otherwise = checkBase' b xs

toNum :: Integral a => a -> [a] -> a
toNum = (. reverse) . toNum'
    where toNum' _ [] = 0
          toNum' b (x:xs) = x + b * toNum' b xs

fromNum :: Integral a => a -> a -> [a]
fromNum = (reverse .) . fromNum'
    where fromNum' _ 0 = []
          fromNum' b n = (n `mod` b) : fromNum' b (n `div` b)
