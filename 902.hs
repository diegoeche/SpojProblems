{-# OPTIONS_GHC -O3 #-}
{-# LANGUAGE BangPatterns #-}


import Data.Array
-- import Data.Array.Base (unsafeAt)
import Debug.Trace
import Control.Applicative

values = takeWhile (<= 5.208) $ scanl (+) 0 [1/x | x <- [2..]]
l      = length values

lArr = listArray (0,l-1) values

search v = binSearch 0 (l-1)
       where binSearch l h | lArr ! idx > v && lArr ! (idx -1) < v = idx
                           | lArr ! idx > v && lArr ! (idx -1) > v = binSearch l idx
                           | lArr ! idx < v  = binSearch idx h
               where idx =  (l + ((h - l) `div` 2))

main = do
     ls <- takeWhile (/= "0.00") . lines <$> getContents
     mapM_ (putStrLn . (++ " card(s)") . show . search . read) ls
     return ()
