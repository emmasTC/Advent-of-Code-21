import System.Environment
import System.IO

commandTotal:: [[String]] -> String -> Int
commandTotal [] _ = 0
commandTotal (x:xs) command = getNumber x command + commandTotal xs command

getNumber:: [String] -> String -> Int
getNumber [] _ = 0
getNumber (x: xs) command
 | x == command = (read::String->Int) (head xs)
 | otherwise = 0

main = do (fileName:_) <- getArgs
          contents <- readFile fileName
          let linesOfFiles = map words (lines contents)
          print $ (commandTotal linesOfFiles "forward") * ((commandTotal linesOfFiles "down") - (commandTotal linesOfFiles "up"))
