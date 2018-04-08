-- http://www.codewars.com/kata/simple-parenthesis-remova

module SimpleParen where 

solve :: String -> String
solve "" = ""
solve xs = tailWhen (== '+') . reverse $ solve' [0] "" ('(':xs)
  where tailWhen :: (a -> Bool) -> [a] -> [a]
        tailWhen _ [] = []
        tailWhen f arr@(x:xs)
          | f x = xs
          | otherwise = arr
        solve' :: [Int] -> String -> String -> String
        solve' _ result "" = result
        solve' (_:ops) result (')':s) = solve' ops result s
        solve' opStack@(op:_) result ('+':'(':c:s)
          | c == '-' || c == '+' || c == '(' = solve' (op:opStack) result (c:s)
          | otherwise = solve' (op:opStack) result ('+':c:s)
        solve' opStack@(op:_) result ('-':'(':c:s)
          | c == '-' || c == '+' || c == '(' = solve' ((op + 1):opStack) result (c:s)
          | otherwise = solve' ((op + 1):opStack) result ('+':c:s)
        solve' opStack@(op:_) result ('(':c:s)
          | c == '-' || c == '+' || c == '(' = solve' (op:opStack) result (c:s)
          | otherwise = solve' (op:opStack) result ('+':c:s)
        solve' opStack@(op:_) result ('+':s)
          | odd op = solve' opStack ('-':result) s
          | even op = solve' opStack ('+':result) s
        solve' opStack@(op:_) result ('-':s)
          | odd op = solve' opStack ('+':result) s
          | even op = solve' opStack ('-':result) s
        solve' opStack result (x:xs) = solve' opStack (x:result) xs
