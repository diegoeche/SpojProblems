{-# OPTIONS_GHC -O3 #-}
{-# LANGUAGE BangPatterns #-}

import Control.Applicative
import Data.Function
import Test.QuickCheck

solve a b | b == "0" = "1"
          | otherwise = byA $ last a
  where byA '0' = "0"
        byA '1' = "1"
        byA '5' = "5"
        byA '6' = "6"
        byA '2' = rd4 "2486" b
        byA '3' = rd4 "3971" b
        byA '7' = rd4 "7931" b
        byA '8' = rd4 "8426" b
        byA '4' = rd2 "46" b
        byA '9' = rd2 "91" b
        rd4 l b = [l !! ((read b - 1) `mod` 4)]
        rd2 l b = [l !! ((read b - 1) `mod` 2)]

specification a b = (:[]) . last $ show (a ^ b)

prop a b = b > 0
           ==> specification a b == ((solve `on` show) a b)

main = do
     l:ls <- lines <$> getContents
     mapM_ (putStrLn . uncurry solve) $ map ((\[x,y] -> (x,y)) . words) $ take (read l) ls
     return ()


