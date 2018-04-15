module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , simulate
    , turnLeft
    , turnRight
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)

data Robot = Robot Bearing (Integer, Integer) deriving (Eq, Show)

bearing :: Robot -> Bearing
bearing (Robot b _) = b

coordinates :: Robot -> (Integer, Integer)
coordinates (Robot _ p) = p

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

simulate :: Robot -> String -> Robot
simulate robot [] = robot
simulate (Robot b p) ('L':xs) = simulate (Robot (turnLeft b) p) xs
simulate (Robot b p) ('R':xs) = simulate (Robot (turnRight b) p) xs
simulate (Robot North (x, y)) ('A':xs) = simulate (Robot North (x, y + 1)) xs
simulate (Robot South (x, y)) ('A':xs) = simulate (Robot South (x, y - 1)) xs
simulate (Robot East (x, y)) ('A':xs) = simulate (Robot East (x + 1, y)) xs
simulate (Robot West (x, y)) ('A':xs) = simulate (Robot West (x - 1, y)) xs

turnLeft :: Bearing -> Bearing
turnLeft North = West
turnLeft West = South
turnLeft South = East
turnLeft East = North

turnRight :: Bearing -> Bearing
turnRight West = North
turnRight South = West
turnRight East = South
turnRight North = East
