import Data.List

fact 0 = 1
fact n | n > 0 = n * fact (n - 1)

-- countZeros' :: Integer -> Int
-- countZeros' = length . takeWhile (== '0') . reverse . show . fact

-- countZeros :: (Integral a) => a -> a
-- countZeros n =  (n `div` 5) + (n `div` 25) + (n `div` 125) + (n `div` 625) + (n `div` 3125) 

-- countZeros :: Integer -> Integer
-- countZeros n = foldr ((+) . (n `div`)) 0  $ takeWhile (n>=) [(5^x) | x <- [1..]]

-- test n = map countZeros' [1..n] == map (fromIntegral . countZeros) [1..n] 

fact n = product [1..n]

main :: IO ()
main = do
     x:xs <- fmap lines getContents
     mapM_ (print . fact . read) xs
--     print $ countZeros' 200
     -- print $ otherVariable 500
     