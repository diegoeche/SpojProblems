import Control.Applicative
import Control.Monad.State
import Data.List

takeList n xs =
         take (length xs `div` n)
         $ evalState (mapM (State . splitAt) $ repeat n) xs

byPairs []       = []
byPairs (x:y:xs) = (x,y):byPairs xs

reverseEven :: [[a]] -> [[a]]
reverseEven xs = fst $ runState (mapM t xs) False
            where t xs =  do
                  x <- get
                  modify not
                  return $ if x then reverse xs else xs 

solve :: String -> [a] -> [a]
solve x = concat . transpose . reverseEven . takeList (read x)

main = do
     ls <- byPairs . takeWhile (/= "0") . lines <$> getContents
     mapM_ (putStrLn . uncurry solve) ls
     return ()