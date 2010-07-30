{-# OPTIONS_GHC -O3 #-}
{-# LANGUAGE BangPatterns #-}
import Data.List
import Control.Monad
import Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Function

takeInt (Just (!x,y)) = x

solve x =  sum .  (zipWith (*) `on` sort) x

takeRatings [] = []
takeRatings (_:x:y:xs) = (asList x, asList y): takeRatings xs
 where asList =  map (takeInt . B.readInt) . B.words

main = do
     _:ls <-  B.lines <$> B.getContents
     mapM_ (print . uncurry solve) $ takeRatings ls
     return ()
