import System.Environment
import System.IO

commandTotal:: [[String]] -> String -> Int
commandTotal [] _ = 0
commandTotal (x:xs) command = getNumber x command + commandTotal xs command

getNumber:: [String] -> String -> Int
getNumber [] _ = 0
getNumber (x:xs) command
 | x == command = (read::String->Int) (head xs)
 | otherwise = 0

aimDepth:: [[String]] -> Int -> Int -> Int
aimDepth [] _ _= 0
aimDepth (x:xs) d aim
  | head x == "forward" = ((aim * (getNumber x "forward"))) + aimDepth xs (d + (aim * (getNumber x "forward"))) aim
  | head x == "down"    = aimDepth xs d (aim + (getNumber x "down"))
  | head x == "up"      = aimDepth xs d (aim - (getNumber x "up"))
  | otherwise = 0


main = do (fileName:_) <- getArgs
          contents <- readFile fileName
          let linesOfFiles = map words (lines contents)
          print $ aimDepth linesOfFiles 0 0
          print $ (commandTotal linesOfFiles "forward") * (aimDepth linesOfFiles 0 0)
