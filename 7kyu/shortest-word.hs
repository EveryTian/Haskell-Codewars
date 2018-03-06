-- https://www.codewars.com/kata/shortest-word

module FindShortest where
find_shortest :: String -> Integer
find_shortest x = fromIntegral $ minimum (map length (words x))
