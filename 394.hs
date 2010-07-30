{-# OPTIONS_GHC -O3 #-}
{-# LANGUAGE BangPatterns #-}

import Control.Applicative
import Control.Monad
import Data.Function
import qualified Data.ByteString.Char8 as B
-- import Test.QuickCheck
-- import Data.Char

-- data Code = Code String
          

-- instance Show Code where
--     show (Code x) = x

-- (Code x) +++ (Code y) = Code (x ++ y)

-- instance Arbitrary Code where
--   arbitrary = sized expr'
--     where expr' 0 = Code <$> (oneof [return . show $ ((-) `on` fromEnum) x 'a' + 1 | x <- ['a'..'z']])
-- 	  expr' n | n > 0 = (+++) <$> expr' (n-1) <*> expr' (n-1)
--   shrink x = []
-- prop :: Code -> Bool
-- prop x = solve sx == solve' sx
--     where sx = show x
-- tests = ["25114",
--          "1111111111",
--          "3333333333"]


main = do
     ls <- takeWhile (/= "0") . lines <$> getContents
     mapM_ (print . solve') ls
     return ()

fibs = 1:1: zipWith (+) fibs (tail fibs)

split accum (_:'0':xs)
    | accum > 0 = accum : split 0 xs
    | otherwise = split 0 xs
split accum ('2':x':xs)
    | x' <= '2' = split (accum + 1) (x':xs)
    | x' <= '6' = (accum + 2): split 0 xs
    | otherwise = (accum + 1): split 0 xs
split accum ('1':x':xs)
    | x' <= '2' = split (accum + 1) (x':xs)
    | otherwise = (accum + 2): split 0 xs
split accum (_:xs)      | accum > 0 = (1 + accum): split 0 xs
split accum (_:xs)      | otherwise = split 0 xs
split accum _           | accum > 0 = accum: []
split accum _           | otherwise = []



solve' = product . map (fibs!!) . split 0

solve (_:'0':xs) = solve xs
solve ('2':x':xs) | x' <= '6' = solve (x':xs) + solve xs
                  | otherwise = solve xs
solve ('1':x':xs) = solve (x':xs) + solve xs
solve ( _ :x':xs) = solve (x':xs)
solve _ = 1


