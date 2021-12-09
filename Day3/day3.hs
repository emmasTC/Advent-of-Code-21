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

oxygenGen:: [String] -> Int -> [String]
oxygenGen [] _ = []
oxygenGen bNumList index
  | length bNumList == 1 = bNumList
  | length bNumList > 1 = if (fromIntegral (noOfOnes bNumList index)) >= ((/) ((fromIntegral) (length bNumList)) 2)
                              then oxygenGen (filter (\n -> (drop (index - 1) (take index n)) == "1") bNumList) (index + 1)
                           else oxygenGen (filter (\n -> (drop (index - 1) (take index n)) == "0") bNumList) (index + 1)

noOfOnes list index = length (filter (\n -> (drop (index - 1) (take index n)) == "1") list)

c02Scrub:: [String] -> Int -> [String]
c02Scrub [] _ = []
c02Scrub bNumList index
  | length bNumList == 1 = bNumList
  | length bNumList > 1 = if (fromIntegral (noOfOnes bNumList index)) >= ((/) ((fromIntegral) (length bNumList)) 2)
                                        then c02Scrub (filter (\n -> (drop (index - 1) (take index n)) == "0") bNumList) (index + 1)
                                     else c02Scrub (filter (\n -> (drop (index - 1) (take index n)) == "1") bNumList) (index + 1)

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

          -- Part 1
          print $ (binToDec gammaZip) * (binToDec epsilonZip)

          -- Part 2
          let oxyZip = zip (head (oxygenGen strbnums 1)) [12,11..]
          let c02Zip = zip (head (c02Scrub strbnums 1)) [12,11..]
          print $ (binToDec oxyZip) * (binToDec c02Zip)
