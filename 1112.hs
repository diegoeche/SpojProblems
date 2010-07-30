import Control.Applicative

toPair :: String -> (Integer, Integer)
toPair = (\[x,y] -> (x,y)) . map read . words

solve x y | x - y == 0 || x - y == 2 = Just $ if even x then x + y else x + y - 1
          | otherwise = Nothing

pp (Just x) = show x
pp Nothing  = "No Number"

main = do
     _:ls <- lines <$> getContents
     mapM_ (putStrLn . pp . uncurry solve . toPair) ls
     return ()