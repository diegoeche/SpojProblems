{-# OPTIONS_GHC -O3 #-}
{-# LANGUAGE BangPatterns #-}

import Control.Applicative
import Control.Monad
import Data.Function
import qualified Data.ByteString.Char8 as B
import Data.Array 
import Data.Bits

-- s 0 0 = 1
-- s n 0 | n > 0 = 0
-- s 0 m | m > 0 = 0
-- s n m |   n > 0
--         && m > 0 = m * s (n-1) m + s(n-1) (m-1)

-- specification n m = s n m `mod` 2

-- m * s (n-1) m + s(n-1) (m-1) ; m is odd
-- m * (m * s (n-2) m + s(n-2) (m-1)) + ((m-1) * s (n-2) (m-1) + s(n-2) (m-2))
-- m * (m * s (n-2) m + s(n-2) (m-1)) + s(n-2) (m-2)

-- s (n-2) m                + s(n-2)(m-1)               + s(n-2) (m-2)
-- s (n-3) m + s(n-3)(m-1)  + s(n-3)(m-1) + s(n-3)(m-2) + s(n-3)(m-2) + s(n-3)(m-3)
-- s (n-3) m +                                                        + s(n-3)(m-3)

-- op x y = if x == y then 0 else 1

-- arr = listArray ((0,0), (max',max')) values
--     where
--       max' = 500
--       values = do
--             n <- [0..max']
--             m <- [0..max']
--             if n == m
--                 then return 1
--                 else if m>n || m==0 || n==0 then return 0
--                      else if even m
--                           then return $ arr ! (n-1,m-1)
--                           else return $ (arr ! ((n-1), m)) + (arr ! (n-1,m-1))

-- -- -- return $ (arr ! ((n-1), m)) `op` (arr ! (n-1,m-1))
-- memoizing_sb = 

-- sb n m = s' n m `mod` 2
--     where s' 0 0 = 1
--           s' n 0 | n > 0 = 0
--           s' 0 m | m > 0 = 0
--           s' n m
--               | m == n = 1
--               | n > 0
--                 && m > 0 =
--                     if even m then s' (n-1) (m-1)
--                     else  s'(n-1) m + s'(n-1) (m-1)

-- tests = do
--   x <- [1..15]
--   y <- [1..x]
--   return (x,y)

sb :: Int -> Int -> Int
sb !n !m  = if (n-m) .&. ((m-1) `div` 2) == 0 then 1 else 0

readInts :: B.ByteString -> Maybe (Int, Int)
readInts x = do
  (n,rest) <- B.readInt x
  (m,_) <- B.readInt $ B.tail rest
  return (n,m)

getInts = maybe (0,0) id . readInts

main = do
     l:ls <- B.lines <$> B.getContents
     mapM_ (print . solve' . getInts) $ take (read . B.unpack $ l) ls
     return ()

solve' = uncurry sb
