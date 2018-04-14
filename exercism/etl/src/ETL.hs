module ETL (transform) where

import Data.Map (Map)
import Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Char (toLower)

transform :: Ord a => Map a String -> Map Char a
transform legacyData = f (M.keys legacyData) (M.fromList [])
  where f [] r = r
        f (x:xs) r = f xs $ g x (fromMaybe "" (M.lookup x legacyData)) r
          where g _ [] r = r
                g a (x:xs) r = g a xs $ M.insert (toLower x) a r
