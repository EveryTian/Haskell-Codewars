-- https://www.codewars.com/kata/human-readable-duration-format

module FormatDuration where

formatDuration :: (Integral i, Show i) => i -> String
formatDuration 0 = "now"
formatDuration n = 
    let hour = n `quot` 3600 `rem` 24
        minute = n `quot` 60 `rem` 60
        second = n `rem` 60
        day = n `quot` (3600 * 24) `rem` 365
        year = n `quot` (3600 * 24 * 365)
    in g $ filter (not . null) $ zipWith f [year, day, hour, minute, second] ["year", "day", "hour", "minute", "second"]
    where f 0 _ = ""
          f 1 s = '1':' ':s
          f n s = show n ++ ' ':s ++ "s"
          g [] = ""
          g [s] = s
          g [s1, s2] = s1 ++ " and " ++ s2
          g (x:xs) = x ++ ", " ++ g xs
