import System.Environment
import Data.Char(digitToInt)

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
binToDec (x:xs) = (digitToInt (fst x)) * (2 ^ ((snd x)-1)) + binToDec xs

main = do (fileName:_) <- getArgs
          contents <- readFile fileName
          let strbnums = (lines contents)
          let listLen = length strbnums
          let onesList = reverse $ totalOnes strbnums 12
          let gammaBin =  gamma onesList listLen
          let epsilonBin = epsilon onesList listLen
          let gammaZip = zip gammaBin [12,11..]
          let epsilonZip = zip epsilonBin [12,11..]
          print $ (binToDec gammaZip) * (binToDec epsilonZip)
        
