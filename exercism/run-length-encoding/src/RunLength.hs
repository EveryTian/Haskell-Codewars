module RunLength (decode, encode) where

import Data.List (group, groupBy)
import Data.Char (isDigit)
import Data.Function (on)

decode :: String -> String
decode = f . groupBy ((&&) `on` isDigit)
    where f [] = ""
          f [x] = x
          f [x, y] = if isDigit $ head x then replicate (read x) (head y) else x ++ y
          f (x:y:zs) = if isDigit $ head x 
                       then replicate (read x) (head y) ++ f zs
                       else x ++ f (y:zs)

encode :: String -> String
encode = concatMap (\ x -> let len = length x
                           in if len == 1 then [head x] 
                              else show len ++ [head x]) . group
