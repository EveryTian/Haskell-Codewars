module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, toGregorian)
import Data.Time.Calendar.OrdinalDate (mondayStartWeek)

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
             deriving Eq

dayToWeekday :: Day -> Weekday
dayToWeekday day = case snd $ mondayStartWeek day of
    1 -> Monday
    2 -> Tuesday
    3 -> Wednesday
    4 -> Thursday
    5 -> Friday
    6 -> Saturday
    7 -> Sunday

isWeekday :: Day -> Weekday -> Bool
isWeekday day weekday = dayToWeekday day == weekday

isTeen :: Day -> Bool
isTeen d
    | n `elem` [13..19] = True
    | otherwise         = False
    where (_, _, n) = toGregorian d

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month = case schedule of
    First   -> head ds
    Second  -> ds !! 1
    Third   -> ds !! 2
    Fourth  -> ds !! 3
    Last    -> last ds
    Teenth  -> head $ filter isTeen ds
    where ds = filter (`isWeekday` weekday) $ map (fromGregorian year month) [1..31]
