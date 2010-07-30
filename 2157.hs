import Control.Monad
import Control.Applicative
import qualified Control.Exception as E
import Data.List

safeRead :: String -> Maybe Int
safeRead s =
         case reads s of
           [(x, "")] -> Just x
           _ -> Nothing

getABC = (\ [a,_,b,_,c] -> (a,b,c)) . words

monadPowers (a,b,c) =
    let [ma',mb',mc'] = map safeRead [a,b,c]
        ma'' = (-) <$> mc' <*> mb'
        mb'' = (-) <$> mc' <*> ma'
        mc'' = (+) <$> ma' <*> mb'
    in maybe [] id $ zipWithM mplus [ma',mb',mc'] [ma'',mb'',mc'']

pp [a,b,c] = concat [show a, " + ", show b, " = ", show c]

getOddLines = map snd . filter fst . zip (cycle [True,False])

main = do
    l:ls <- getOddLines . lines <$> getContents
    mapM_ (putStrLn . pp . monadPowers . getABC) $ take (read l) ls
    return ()
