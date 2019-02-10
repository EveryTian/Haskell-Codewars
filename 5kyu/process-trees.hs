-- https://www.codewars.com/kata/process-trees

module ProcessTree where
import ProcessTree.Process

{- preloaded:
type PID = Int
data Process = Process PID [Process]
-}

makeTree :: [(PID, PID)] -> Process
makeTree = f 1
    where f p xs = Process p [f (fst i) xs | i <- xs, snd i == p]
