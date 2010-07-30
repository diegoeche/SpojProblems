{-# OPTIONS_GHC -O3 #-}
{-# LANGUAGE BangPatterns #-}

import Data.List
import Control.Monad
import Control.Applicative
import Data.Array
import qualified Data.ByteString.Lazy.Char8 as B

primes :: [Int]
primes = 2: 3: sieve 0 (tail primes) 5
sieve k (p:ps) x = [n | n <- [x,x+2..p*p-2], and [n `rem`p /=0 | p <- fs]]
                     ++ sieve (k+1) ps (p*p+2)
                     where fs = take k (tail primes)


memoized_divisors' :: Int -> [Int]
memoized_divisors' n | n > size = divisors' n
                     | otherwise = memoArray ! n
 where size = 100000
       memoArray = listArray (0,size) (map divisors' [0 .. size])
       -- divisors'  = map product
       --               . init
       --               . sequence
       --               . map (scanl (*) 1)
       --               . group
       --               . primeFactors


divisors' :: Int -> [Int]
divisors'  = map product
             . init
             . mapM (scanl (*) 1)
             . group
             . primeFactors


primeFactors :: Int -> [Int]
primeFactors 1       = []
primeFactors n = foo n primes
     where
       size = 100000
       memoArray = listArray (0,size) (map (memoGo primes) [0 .. size])
       foo n ps@(p:pt)
         | n <= size       = memoArray !  n
         | p*p > n        = [n]
         | n `rem` p == 0  = p:foo (n `quot` p) ps
         | otherwise      = memoGo pt n
       memoGo ps@(p:pt) n
        | p*p > n        = [n]
        | n `rem` p == 0  = p:memoGo ps (n `quot` p)
        | otherwise      = memoGo pt n

-- primeFactors' 1 = []
-- primeFactors' n = go n primes
--   where
--        go n ps@(p:pt)
--         | p*p > n        = [n]
--         | n `rem` p == 0  = p : go (n `quot` p) ps
--         | otherwise      = go n pt


-- divisors :: Integer -> [Integer]
-- divisors 1 = []
-- divisors n = -- (1:)
--            nub
--            . map product
--            . tail
--            . powerset'
-- --           . filterM (const [False,True])
--            $ primeFactors n

-- powerset' [] = [[]]
-- powerset' (x:xs) = concatMap (\ys -> [x:ys,ys]) (powerset' xs)

main = do
     _:ls <- B.lines <$> B.getContents
     mapM_ (print . sum . memoized_divisors' . takeInt . B.readInt) ls
     return ()


takeInt (Just (!x,y)) = x

