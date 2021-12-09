import System.Environment

totalOnes:: [String] -> Int -> [Int]
totalOnes [] _ = []
totalOnes list characters
  | characters == 0 = []
  | characters /= 0 = length (filter (\n -> (drop (characters - 1) (take characters n)) == "1") list) : totalOnes list (characters-1)

gamma ::[Int] -> Int -> String
gamma [] _ = ""
gamma (x:xs) listLen =
   if x > (div listLen 2)
      then "1" ++ gamma xs listLen
   else "0" ++ gamma xs listLen

epsilon ::[Int] -> Int -> String
epsilon [] _ = ""
epsilon (x:xs) listLen =
  if x > (div listLen 2)
     then "0" ++ epsilon xs listLen
  else "1" ++ epsilon xs listLen

binToDec :: [(Char, Int)] -> Int
binToDec [] = 0
binToDec (x:xs) = 2 ^ ((snd x)-1) + binToDec xs

main = do (fileName:_) <- getArgs
          contents <- readFile fileName
          let strbnums = (lines contents)
          print $ strbnums
          let listLen = length strbnums
          print $ listLen
          print $ length (filter (\n -> (drop (1 - 1) (take 1 n)) == "1") strbnums)
          let onesList = reverse $ totalOnes strbnums 12
          print $ gamma onesList listLen
          print $ epsilon onesList listLen
          let gammaBin =  gamma onesList listLen
          let epsilonBin = epsilon onesList listLen
          print $ zip gammaBin [12,11..]
          let gammaZip = zip gammaBin [12,11..]
          let binOneList= (filter (\n -> n == ('1', 1)) gammaZip) ++ (filter (\n -> n == ('1', 2)) gammaZip) ++ (filter (\n -> n == ('1', 3)) gammaZip) ++ (filter (\n -> n == ('1', 4)) gammaZip) ++ (filter (\n -> n == ('1', 5)) gammaZip) ++ (filter (\n -> n == ('1', 6)) gammaZip) ++ (filter (\n -> n == ('1', 7)) gammaZip) ++ (filter (\n -> n == ('1', 8)) gammaZip) ++ (filter (\n -> n == ('1', 9)) gammaZip) ++ (filter (\n -> n == ('1', 10)) gammaZip) ++ (filter (\n -> n == ('1', 11)) gammaZip) ++ (filter (\n -> n == ('1', 12)) gammaZip)
          print $ binToDec binOneList
          --print $ getInt (head binOneList)
          --print $ binToDec gammaZip 5
          --print $ filter (\n -> n == ('1', [1..])) gammaZip
