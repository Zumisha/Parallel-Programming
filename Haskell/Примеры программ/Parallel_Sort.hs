import Control.Parallel.Strategies
import Data.Time
import System.Random
import Control.Parallel
import Data.List
import System.Environment
import System.IO
import Text.Printf


quickSort :: (Ord a) => [a] -> [a]  
quickSort [] = []  
quickSort (h:t) =   
        let 
            smallerH = quickSort [x | x <- t, x <= h]  
            biggerH = quickSort [x | x <- t, x > h]  
       in smallerH ++ [h] ++ biggerH


merge :: (Ord a) => [a] -> [a] -> [a]
merge [] x = x
merge x [] = x
merge (x:xs) (y:ys) 
        | y < x     = y : merge (x:xs) ys
        | otherwise = x : merge xs (y:ys)


mergeSubList :: (Ord a) => [[a]] -> [a]
mergeSubList (h:t)
        | length t == 0 = h
        | otherwise = merge h (mergeSubList t)


runn :: Int -> [Double] -> [[Double]]
runn p [] = []
runn p lst = (take lenSubList lst):runn (p - 1) (drop lenSubList lst) 
    where lenSubList = div (length lst) p


run :: [[Double]] -> [[Double]]
run lst = let subsegmentIntegral = map quickSort lst `using` parList rdeepseq in subsegmentIntegral

readDouble :: String -> Double
readDouble = read

main :: IO ()
main = do
    start <- getCurrentTime
    (p: fileName: _) <- getArgs
    contents <- readFile fileName
    let a = map readDouble . words $ contents
    printf "Inp len: %d\n" (length a)
    readEnd <- getCurrentTime
    printf "Read time: %.3f.\n" (realToFrac (diffUTCTime readEnd start):: Double)
    let b = mergeSubList $ run $ runn (read p :: Int) a
    printf "Res len: %d\n" (length b)
    sortEnd <- getCurrentTime
    printf "Sort time: %.3f\n" (realToFrac (diffUTCTime sortEnd readEnd):: Double)
    writeFile "output.txt" $ show $ b --запись в файл
    end <- getCurrentTime
    printf "Write time: %.3f\n" (realToFrac (diffUTCTime end sortEnd):: Double)
    printf "Total time: %.3f\n" (realToFrac (diffUTCTime end start):: Double)