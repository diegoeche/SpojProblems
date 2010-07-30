
add :: [Integer] -> String
add [x, y] = dropWhile (== '0') . reverse $ show $ x + y

main = do
     _:xs <- fmap lines getContents
     mapM_ (putStrLn . add . (map (read . reverse)) . words) xs
