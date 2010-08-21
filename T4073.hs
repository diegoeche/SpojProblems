{-# OPTIONS_GHC -O3 #-}
{-# LANGUAGE BangPatterns #-}


import Control.Monad
import Control.Monad.Identity
import Control.Applicative
import Data.Array

-- Simple Version
cycleLength' :: Int -> Int
cycleLength' 1 = 1
cycleLength' n | odd n     = 1 + cycleLength' (3 * n + 1)
               | otherwise = 1 + cycleLength' (n `div` 2)

cycleLength :: Int -> Int
-- Tailrec
cycleLength = loop 1
    where loop !accum 1 = accum
          loop !accum n | odd n     = loop (accum + 1) (3 * n + 1)
                        | otherwise = loop (accum + 1) (n `div` 2)


memoized :: Int -> Int
memoized  = solve'
  where solve' n | n <= top = (memoArray ! n)
                 | otherwise = cycleLength n
        top = 1000
        !memoArray = listArray (1,top) [cycleLength x | x <- [1..top]]

solve :: Int -> Int -> Int
solve i j | i <= j     = maximum . map memoized $ [i..j]
          | otherwise = solve j i

parseCase :: String -> (Int, Int)
parseCase = toInts . words
            where toInts [x,y] = (read x, read y)

outputSolution :: String -> String
outputSolution s = s ++ " " ++ f s
    where f = show . uncurry solve . parseCase


main :: IO ()
main =  do
    cases <- lines <$> getContents
    mapM_ (putStrLn . outputSolution) cases



toInts' [x, y] = (read x, read y)

