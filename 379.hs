{-# OPTIONS_GHC -O3 #-}
{-# LANGUAGE BangPatterns #-}

import Data.List
import Control.Monad
import Control.Applicative
import Data.Array.ST
import Control.Monad.ST
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Array
import Data.Array.Base (unsafeAt)

solve xs = let !top = length xs - 1
               arr = listArray (0,top) xs
               vs  = map (\idx -> idx+1 == (arr `unsafeAt` ((arr `unsafeAt` idx) - 1))) [0..top]
           in and vs

parse = map (takeInt . B.readInt) . B.words

takeInt (Just (!x,y)) = x

pp True = "ambiguous"
pp False = "not ambiguous"

getPerm [] = []
getPerm (_:x:xs) = x: getPerm xs

main = do
     ls <- getPerm . takeWhile (/= B.pack "0") . B.lines <$> B.getContents
     mapM_ (putStrLn . pp . solve . parse) ls
     return ()
