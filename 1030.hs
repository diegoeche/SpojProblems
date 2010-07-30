{-# OPTIONS_GHC -O3 #-}
{-# LANGUAGE BangPatterns #-}

import Control.Applicative
import qualified Data.ByteString.Char8 as B

-- isFat x =
--       case reverse (show x) of
--         ('8':'8':'8':xs) -> True
--         _                -> False

-- cubes = map fst $ filter (isFat . snd) [(x, x*x*x) | x <- [1..100000]]

solve n =
      let m = n `mod` 4
          firstPart | n >= 5  = B.pack . show $ (n - 1) `div` 4
                    | otherwise  = B.empty
          lastPart 1 = B.pack "192"
          lastPart 2 = B.pack "442"
          lastPart 3 = B.pack "692"
          lastPart 0 = B.pack "942"
      in  firstPart `B.append` (lastPart m)

takeInteger (Just (!x,y)) = x

main = do
     _:ls <- B.lines <$> B.getContents
     mapM_ (B.putStrLn . solve . takeInteger) $ map B.readInteger ls
     return ()
