{-# OPTIONS_GHC -O3 #-}
{-# LANGUAGE BangPatterns #-}

import Control.Applicative ((<$>))
import Data.List
import Data.Maybe
import Data.Array

f :: Integer -> Integer -> Integer
f n 1 = n
f n !k =
    let !a = f n (k-1)
    in if odd a then
           3 * a + 1
       else
           a `div` 2

-- solve :: Integer -> Int
-- solve n = (+1) . fromJust . findIndex (== 1) . map (f n) $ [1..]

cycleLength :: Integer -> Integer
-- Tailrec
cycleLength = loop 1
    where loop !accum 1 = accum
          loop !accum n | odd n     = loop (accum + 1) (3 * n + 1)
                        | otherwise = loop (accum + 1) (n `div` 2)


memoized :: Integer -> Integer
memoized  = solve'
  where solve' n | n <= top = (memoArray ! n)
                 | otherwise = cycleLength n
        top = 1000
        !memoArray = listArray (1,top) [cycleLength x | x <- [1..top]]


main = do
     cases <- lines <$> getContents
     mapM_ (putStrLn . show . memoized . read) cases
     return ()
