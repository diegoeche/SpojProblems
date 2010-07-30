{-# OPTIONS_GHC -O3 #-}
{-# LANGUAGE BangPatterns #-}

import Data.List
import Control.Applicative
import Control.Arrow
--import qualified Data.Map as M
import Data.Array

solve :: Integer -> Integer
solve  = solve'
  where solve' n | n <= top = (memoArray ! n) 
                 | otherwise = f n
        top = 250000
        memoArray = listArray (0,top) [f x | x <- [0..top]]
        f 0 = 0
        f n = max n (sum [solve $ n `div` x | x <-[2..4]])

main = do
    ls <- map read . lines <$> getContents
    mapM_ (print . solve) ls
    return ()

