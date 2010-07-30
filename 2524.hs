{-# OPTIONS_GHC -O3 #-}
{-# LANGUAGE BangPatterns #-}

import Control.Monad
import Control.Applicative
import Text.Printf

parse :: String -> (Double, String)
parse = (\[x,y] -> (read x, y)) . words

solve :: Double -> String -> String
solve x units | units == "kg" = printf "%.4f lb" (x * 2.2046)
              | units == "lb" = printf "%.4f kg" (x * 0.4536)
              | units == "g"  = printf "%.4f l"  (x * 3.7854)
              | units == "l"  = printf "%.4f g"  (x * 0.2642)

main = do
     x:ls <- lines <$> getContents
     zipWithM_ (\x y ->
                   putStrLn
                   $ unwords [show x,
                              uncurry solve $ parse y]) [1..] (take (read x) $ ls)
     return ()