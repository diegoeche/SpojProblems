{-# OPTIONS_GHC -O3 #-}
{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Control.Applicative
import qualified Data.ByteString.Char8 as B

takeInt (Just (!x,y)) = x

remove _ [] = []
remove 1 (x:xs) = xs
remove !n (x:xs) | n > 1 = x:remove (n-1) xs

parse = (\(x:y:[]) -> (read x, y)) . words

solve x y = show x ++ ' ':(uncurry remove $ parse y)


main = do
     _:ls <- lines <$> getContents
     zipWithM ((putStrLn .) . solve) [1..] ls
     return ()
