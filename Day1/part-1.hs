import System.Environment
import System.IO

myFun :: [Int] -> Int -> [Int]
myFun [] _ = []
myFun (x:xs) a
 | x > a = x : myFun xs x
 | otherwise = myFun xs x

main = do (fileName:_) <- getArgs
          contents <- readFile fileName
          let numList = map (read::String->Int) (words contents)
          print $ length (myFun numList 0) -1
