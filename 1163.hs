{-# OPTIONS_GHC -O2 #-}

import Control.Applicative
import Data.Char
import Control.Monad
import Data.List

data Language = Java | Cpp | DontKnow

solve = start >=> loop DontKnow
      where start ('_':xs) = fail "Starts with underscore"
            start w@(x:xs) | isUpper x = fail "Starts with uppercase"
                           | otherwise = return w
            loop _        []      = return []
            loop DontKnow [x]     | isLower x = return [x]
                                  | isUpper x = return ['_',toLower x]
            loop _        ([x,y]) | all isLower [x,y] = return [x,y]
                                  | isLower x && isUpper y = return [x,'_', toLower y]
            loop DontKnow (x:xs)
                 | isLower x = liftM (x:) $ loop DontKnow xs
                 | isUpper x = liftM (['_', toLower x] ++) $ loop Java xs
            loop DontKnow ('_':z:xs) | isLower z = liftM (toUpper z:) $ loop Cpp xs

            loop Java (x:xs) | isLower x =  (x:) `liftM` loop Java xs
                             | isUpper x =  liftM (['_',toLower x]++) $ loop Java xs

            loop Cpp  (x:xs) | isLower x =  (x:) `liftM` loop Cpp xs
            loop Cpp  ('_':x:xs) | isLower x =  (toUpper x:) `liftM` loop Cpp xs
            loop _ _ = fail "error"




pp (Just x) = x
pp Nothing  = "Error!"

test = pp . solve

main = do
     ls <- lines <$> getContents
     mapM_ (putStrLn . pp . solve) ls