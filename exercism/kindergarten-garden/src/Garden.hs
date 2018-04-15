module Garden
    ( Plant (..)
    , defaultGarden
    , garden
    , lookupPlants
    ) where

import Data.Map (Map)
import Data.Map as M
import Data.List (sort)
import Data.Maybe (fromMaybe)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

studentsNames :: [String]
studentsNames = ["Alice", "Bob", "Charlie", "David",
                 "Eve", "Fred", "Ginny", "Harriet",
                 "Ileana", "Joseph", "Kincaid", "Larry"]

getStudentIndex :: String -> Int
getStudentIndex name = index name studentsNames 0
    where index _ [] _ = -1
          index name (x:xs) i
              | name == x = i
              | otherwise = index name xs (i + 1)

getPlant :: Char -> Plant
getPlant 'C' = Clover
getPlant 'G' = Grass
getPlant 'R' = Radishes
getPlant 'V' = Violets

defaultGarden :: String -> Map String [Plant]
defaultGarden = garden studentsNames

garden :: [String] -> String -> Map String [Plant]
garden students plantsStr = let plants = Prelude.map (Prelude.map getPlant) $ words plantsStr
                            in M.fromList $ zip (sort students) $ f plants
    where f :: [[Plant]] -> [[Plant]]
          f [[], []] = []
          f [x0:x1:xs, y0:y1:ys] = [x0, x1, y0, y1] : f [xs, ys]

lookupPlants :: String -> Map String [Plant] -> [Plant]
lookupPlants = (fromMaybe [] .) . M.lookup
