-- http://www.codewars.com/kata/simple-string-expansion

module StringExpansion where 

solve :: String -> String
solve "" = ""
solve s@(x:xs)
    | x == '(' = solve ('1':s)
    | x == ')' = ""
    | x `elem` ['a'..'z'] ++ ['A'..'Z'] = x : solve xs
    | x `elem` ['0'..'9'] = 
        let (times, str, remain) = generate s
        in rep times str ++ solve remain

generate :: String -> (Int, String, String)
generate s = generate' s ""
    where generate' ('(':xs) ns = ((read $ reverse ns) :: Int, solve xs, getRemain xs 0)
          generate' (x:xs) ns = generate' xs (x:ns)
          getRemain ('(':xs) n = getRemain xs (n + 1)
          getRemain (')':xs) n
              | n == 0 = xs
              | otherwise = getRemain xs (n - 1)
          getRemain (_:xs) n = getRemain xs n

rep :: Int -> [a] -> [a]
rep x xs
    | x <= 0 = []
    | otherwise = xs ++ rep (x - 1) xs
