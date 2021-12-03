import System.Environment
import System.IO

largerThanPredecessor :: [Int] -> Int -> [Int]
largerThanPredecessor [] _ = []
largerThanPredecessor (x:xs) a
 | x > a     = x : largerThanPredecessor xs x
 | otherwise = largerThanPredecessor xs x



main = do (fileName:_) <- getArgs
          contents <- readFile fileName
          let numList = map (read::String->Int) (words contents)
          print $ length (largerThanPredecessor numList 0) -1 -- need to -1 for the first case as number always greater than 0
