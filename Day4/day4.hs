import System.Environment


stringToIntList::[[String]] -> [[Int]]
stringToIntList [] = []
stringToIntList (x:xs) = map (read::String->Int) x: stringToIntList xs

firstX::[[Int]] -> Int -> [[Int]]
firstX list noElem = take noElem list

getBoards:: [[Int]] -> [[[Int]]]
getBoards [] = []
getBoards list = firstX list 5 : getBoards (drop 5 list)

markElem:: [Int] -> Int -> [Int]
markElem [] _= []
markElem (x:xs) noCalled = if x == noCalled then -1: markElem xs noCalled else x: markElem xs noCalled

markCard:: Int -> [[Int]] -> [[Int]]
markCard _ [] = []
markCard number (x:xs) = markElem x number: markCard number xs

markAllCards:: Int -> [[[Int]]] -> [[[Int]]]
markAllCards _ [] = []
markAllCards number (x:xs) = markCard number x: markAllCards number xs

--playGame::[Int] -> [[[Int]]] -> Int
--playGame [] _ = 0
--playGame _ [] = 0
--playGame numbersCalled (x:xs) = if checkAllCards /= true
--  then markAllCards (take 1 (drop 1 numbersCalled))
--  else take 1 numbersCalled

--checkAllCards

--checkRow

--checkColumn

main = do (fileName:_) <- getArgs
          contents <- readFile fileName
          let strbnums = map words (lines contents)
          let numbersCalled = head strbnums
          let bingoCardsList = tail strbnums
          let filteredList = (filter (\n -> n /= []) (stringToIntList bingoCardsList))
          let boardsList = getBoards filteredList
          let list = [22,13,17,11,0]
          print $ markAllCards 11 boardsList
