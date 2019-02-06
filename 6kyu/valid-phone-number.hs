-- https://www.codewars.com/kata/valid-phone-number

module Codewars.Kata.Phone where

-- import Text.Regex.Posix

-- validPhoneNumber :: String -> Bool
-- validPhoneNumber = ( =~ "\\([0-9]{3}\\) [0-9]{3}-[0-9]{4}")

validPhoneNumber :: String -> Bool
validPhoneNumber = (==) "(ddd) ddd-dddd" . f
    where f "" = ""
          f (x:xs) = (if x `elem` "0123456789" then 'd'
                      else if x == 'd' then '\'' else x) : f xs
