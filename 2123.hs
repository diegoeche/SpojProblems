{-# OPTIONS_GHC -O3 #-}
{-# LANGUAGE BangPatterns #-}

import Data.List
import Control.Applicative
import Control.Arrow

solve xs =
      let (l, s) = (length &&& sum) xs
      in if s `rem` l == 0
         then sum . filter (>0) . map ((s `div` l) `subtract`) $ xs
         else -1

split [] = []
split (x:xs) =
    let (r,l) = splitAt x xs
    in r:split l

main = do
    ls <- split . takeWhile (/= (- 1)) . map read . lines <$> getContents
    mapM_ (print . solve) ls
    return ()
