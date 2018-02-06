-- https://www.codewars.com/kata/simple-fun-number-27-rectangle-rotation

module RectangleRot.JorgeVS.Kata where
rectangleRot :: Int -> Int -> Int
rectangleRot a b = evenMin1 $ f a b
    where get x = floor $ fromIntegral x / sqrt 2
          f a b = get a * get b + (get a + 1) * (get b + 1)
          evenMin1 x = if even x then x - 1 else x
