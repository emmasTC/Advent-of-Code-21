import System.Environment
import System.IO

uninterleave :: [a] -> ([a],[a])
uninterleave = foldr (\x ~(xs,ys) -> (x:ys,xs)) ([],[])

main = do (fileName:_) <- getArgs
          contents <- readFile fileName
          let linesOfFiles = words contents
          print $ linesOfFiles
