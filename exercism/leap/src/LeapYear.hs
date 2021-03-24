module LeapYear (isLeapYear) where

isLeapYear :: Int -> Bool
isLeapYear year
  | isDivisibleBy 400 = True
  | isDivisibleBy 100 = False
  | isDivisibleBy 4   = True
  | otherwise         = False
  where isDivisibleBy x = mod year x == 0

