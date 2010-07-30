import Control.Applicative

toPairs :: [String] -> [(Integer, Integer)]
toPairs [] = []
toPairs (_:[]) = []
toPairs (x:x':xs) = (read x,read x') : toPairs xs

solve (x, y) =
      let klaudia = (x + y) `div` 2
          natalia = x - klaudia
      in (klaudia, natalia)

output (x, y) = do
       print x
       print y

main = do
     ls <- toPairs . lines <$> getContents
     mapM_ (output . solve) ls
     return ()

