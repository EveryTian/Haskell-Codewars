module DNA (nucleotideCounts) where

import Data.Map (Map)
import Data.Map as M
import Data.Maybe (fromMaybe)

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts = g . f (M.fromList [('A', 0), ('T', 0), ('C', 0), ('G', 0)])
    where f :: Map Char Int -> String -> Map Char Int
          f m "" = m
          f m ('A':xs) = f (inc 'A' m) xs
          f m ('T':xs) = f (inc 'T' m) xs
          f m ('C':xs) = f (inc 'C' m) xs
          f m ('G':xs) = f (inc 'G' m) xs
          f _ _ = M.fromList []
          g :: Map Char Int -> Either String (Map Char Int)
          g m = if M.null m then Left "" else Right m
          inc :: Char -> Map Char Int -> Map Char Int
          inc c m = let aft = fromMaybe (-1) (M.lookup c m) + 1
                    in M.insert c aft m
