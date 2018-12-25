-- https://www.codewars.com/kata/calculator

module Calculator where

data CalcEle = AddOp | SubOp | MulOp | DivOp | Num Double deriving (Eq)

instance Show CalcEle where
    show AddOp = "+"
    show SubOp = "-"
    show MulOp = "*"
    show DivOp = "/"
    show (Num n) = show n

instance Read CalcEle where
    readsPrec _ ('+':xs) = [(AddOp, xs)]
    readsPrec _ ('-':xs) = [(SubOp, xs)]
    readsPrec _ ('*':xs) = [(MulOp, xs)]
    readsPrec _ ('/':xs) = [(DivOp, xs)]
    readsPrec n s = [(Num x, y)] where [(x, y)] = readsPrec n s

instance Ord CalcEle where
    Num a `compare` Num b = a `compare` b
    MulOp `compare` AddOp = 1 `compare` 0
    MulOp `compare` SubOp = 1 `compare` 0
    DivOp `compare` AddOp = 1 `compare` 0
    DivOp `compare` SubOp = 1 `compare` 0
    AddOp `compare` MulOp = 0 `compare` 1
    SubOp `compare` MulOp = 0 `compare` 1
    AddOp `compare` DivOp = 0 `compare` 1
    SubOp `compare` DivOp = 0 `compare` 1
    AddOp `compare` SubOp = 0 `compare` 0
    SubOp `compare` AddOp = 0 `compare` 0
    SubOp `compare` SubOp = 0 `compare` 0
    AddOp `compare` AddOp = 0 `compare` 0
    DivOp `compare` MulOp = 1 `compare` 1
    MulOp `compare` DivOp = 1 `compare` 1
    DivOp `compare` DivOp = 1 `compare` 1
    MulOp `compare` MulOp = 1 `compare` 1

evaluate :: String -> Double
evaluate = evalCalcEle . map read . words

evalCalcEle :: [CalcEle] -> Double
evalCalcEle = evalWithStack [] []

evalWithStack :: [Double] -> [CalcEle] -> [CalcEle] -> Double
evalWithStack [] _ [] = 0
evalWithStack [n] _ [] = n
evalWithStack (n0:n1:nn) (x:xs) [] = evalWithStack (apply x n0 n1 : nn) xs [] 
evalWithStack ns [] (y:ys) = case y of
    Num n -> evalWithStack (n:ns) [] ys
    _ -> evalWithStack ns [y] ys
evalWithStack ns ops@(x:xs) eles@(y:ys) = case y of
    Num n -> evalWithStack (n:ns) ops ys
    _ -> if x >= y then let (n0:n1:nn) = ns
                        in evalWithStack (apply x n0 n1 : nn) xs eles
                   else evalWithStack ns (y:ops) ys

apply :: CalcEle -> Double -> Double -> Double
apply AddOp = (+)
apply SubOp = flip (-)
apply MulOp = (*)
apply DivOp = flip (/)

-- For debug:
-- evalDebug :: String -> IO ()
-- evalDebug = eval [] [] . map read . words

-- eval :: [Double] -> [CalcEle] -> [CalcEle] -> IO ()
-- eval [] _ [] = print "-------" >> print 0
-- eval [n] _ [] = print "-------" >> print n
-- eval ns@(n0:n1:nn) ops@(x:xs) [] = do
--     print "-------"
--     print ns
--     print ops
--     eval (apply x n0 n1 : nn) xs [] 
-- eval ns [] eles@(y:ys) = do
--     print "-------"
--     print ns
--     print ""
--     print (y:ys)
--     case y of
--       Num n -> eval (n:ns) [] ys
--       _ -> eval ns [y] ys
-- eval ns ops@(x:xs) eles@(y:ys) = do
--     print "-------"
--     print ns
--     print ops
--     print eles
--     case y of
--       Num n -> eval(n:ns) ops ys
--       _ -> if x >= y then let (n0:n1:nn) = ns
--                           in eval (apply x n0 n1 : nn) xs eles
--                      else eval ns (y:ops) ys

-- Best Practice:
-- data Token = A | S | M | D | V Double
-- evaluate :: String -> Double
-- evaluate = calc id . map tk . words where
--     tk s = case s of "+" -> A; "-" -> S; "*" -> M; "/" -> D; v -> V (read v)
--     calc _ [] = 0
--     calc f [V v] = f v
--     calc f (V v : A : t) = calc (f v +) t
--     calc f (V v : S : t) = calc (f v -) t
--     calc f (V v : M : V u : t) = calc f (V (v * u) : t)
--     calc f (V v : D : V u : t) = calc f (V (v / u) : t)
