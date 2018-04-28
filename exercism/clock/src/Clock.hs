module Clock (clockHour, clockMin, fromHourMin, toString) where

data Clock = Clock {
    clockHour :: Int,
    clockMin :: Int
} deriving Eq

instance Show Clock where
    show = toString

instance Num Clock where
   Clock h1 m1 + Clock h2 m2 = fromHourMin (h1+h2) (m1+m2)
   Clock h1 m1 - Clock h2 m2 = fromHourMin (h1-h2) (m1-m2)
   Clock h1 m1 * Clock h2 m2 = fromHourMin (h1*h2) (m1*m2)
   negate (Clock h m)        = fromHourMin (-h) (-m)
   abs c                     = c
   signum c                  = c
   fromInteger n             = fromHourMin 0 (fromIntegral n)

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = let deltaHour = min `div` 60
                           lastMin = min `mod` 60
                           lastHour = (hour + deltaHour) `mod` 24
                       in Clock lastHour lastMin

toString :: Clock -> String
toString (Clock h m) = f h ++ ":" ++ f m
    where f n = let sn = show n
                in if length sn == 1 then '0':sn else sn
