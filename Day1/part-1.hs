import System.Environment
import System.IO

largerThanPredecessor :: [Int] -> Int -> [Int]
largerThanPredecessor [] _ = []
largerThanPredecessor (x:xs) a
 | x > a     = x : largerThanPredecessor xs x
 | otherwise = largerThanPredecessor xs x

addFirstThree :: [Int] -> Int -> Int -> Int
addFirstThree [] _ _ = 0
addFirstThree (x:xs) accumulator total =
  if accumulator < 3 then addFirstThree xs (accumulator + 1) (total + x) else total

threeList :: [Int] -> [Int]
threeList [] = []
threeList (x:xs) = addFirstThree (x:xs) 0 0: threeList xs

main = do (fileName:_) <- getArgs
          contents <- readFile fileName
          let numList = map (read::String->Int) (words contents)
          print $ length (largerThanPredecessor (threeList numList) 0)
