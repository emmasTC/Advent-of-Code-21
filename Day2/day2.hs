import System.Environment
import System.IO

horizontalPosition :: [Int] -> Int
horizontalPosition x = sum x

getNumber:: [String] -> String -> Int
getNumber [] _ = 0
getNumber (x: xs) command
 | x == command = (read::String->Int) (head xs)
 | otherwise = 0

main = do (fileName:_) <- getArgs
          contents <- readFile fileName
          let linesOfFiles = map words (lines contents)
          print $ linesOfFiles
          print $ getNumber (head linesOfFiles) "forward"
