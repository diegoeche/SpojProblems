{-# OPTIONS_GHC -O3 #-}
{-# LANGUAGE BangPatterns #-}

import Data.List
import Control.Monad
import Control.Applicative
import qualified Data.ByteString.Char8 as B
import Data.Function

f = extract . B.readInteger
    where extract :: Maybe (t, t1) -> t
          extract (Just (!x,y)) = x

solve = uncurry (*) . ( \[x,y] -> (f x, f y) ) . B.words

main = do
     n:ls <-  B.lines <$> B.getContents
     mapM_ (print . solve) $ take (read $ B.unpack n) ls
     return ()

data Empleado = Persona {cedula :: String, nombre :: String }
              deriving Show

