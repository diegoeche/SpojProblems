import Control.Applicative
import qualified Data.ByteString.Char8 as B

{-# OPTIONS_GHC -O3 #-}
{-# LANGUAGE BangPatterns #-}

solve :: Integer -> Integer
solve 1 = 1
solve n = 2 * n - 2

takeInteger (Just (x,y)) = x

main = do
     ls <- B.lines <$> B.getContents
     mapM_ (print . solve . takeInteger) $ map B.readInteger ls
     return ()


