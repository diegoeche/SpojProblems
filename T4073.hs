{-# OPTIONS_GHC -O3 #-}
{-# LANGUAGE BangPatterns #-}

import Control.Monad
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
outputSolution s = s ++ " " ++ show (uncurry solve $ parseCase s)

main :: IO ()
main = do
  cases <- lines `liftM` getContents
  mapM_ (putStrLn . outputSolution) cases
  return ()