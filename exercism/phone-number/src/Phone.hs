module Phone (number) where

number :: String -> Maybe String
number xs = 
    if not . null $ filter (`notElem` '(':')':' ':'-':'.':'+':['0'..'9']) xs
        then Nothing
    else let x = filter (`elem` ['0'..'9']) xs
         in if length x == 11 && head x == '1' && x !! 2 /= '0' && x !! 2 /= '1' && x !! 4 /= '0' && x !! 4 /= '1'
                then Just $ drop 1 x
            else if length x == 10 && head x /= '0' && head x /= '1' && x !! 3 /= '0' && x !! 3 /= '1'
                then Just x
            else Nothing
