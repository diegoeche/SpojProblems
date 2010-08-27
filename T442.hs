{-# OPTIONS_GHC -O3 #-}
{-# LANGUAGE BangPatterns #-}
import Control.Applicative
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import Data.List (span)
-- import Control.Monad.Reader
import Control.Monad (join)
import Control.Arrow (second)
-- import qualified Data.Set as S
import qualified Data.Array as A
import Data.Array.ST
import Control.Monad.ST

type Node = Int
-- type Graph = M.Map Node [Node]
type Graph = A.Array Node [Node]

-- metaSearch2 :: ([Node] -> [Node] -> [Node]) -> Graph -> Node -> [Node]
-- metaSearch2 s g start  =  ms S.empty [start]
--     where ms !marked [] = []
--           ms !marked (n:ns)
--               | n `S.member` marked = ms marked ns
--               | otherwise  =
--                   let children = join . maybeToList $ M.lookup n g
--                   in n: ms (n `S.insert` marked) (ns `s` children)

metaSearch :: ([Node] -> [Node] -> [Node]) -> Graph -> Node -> [Node]
metaSearch s g start  = runST $ ms [start]
    where ms vs = do
            let size = snd $ A.bounds g
            marked <- newListArray (0, size-1) $ repeat False :: ST s (STUArray s Int Bool)
            let loop [] = return []
                loop (n:ns) = do
                    isMarked <- readArray marked (n-1)
                    if isMarked then loop ns
                        else do
                            writeArray marked (n-1) True
--                            let children = join . maybeToList $ M.lookup n g
                            let children = g A.! n
                            (n:) <$> loop (ns `s` children)
            loop vs

bfs :: Graph -> Node -> [Node]
bfs = metaSearch (++)

dfs :: Graph -> Node -> [Node]
dfs = metaSearch $ flip (++)

data Search = BFS | DFS
            deriving Show

type Test = (Int, Search)
type Case = (Graph, [Test])

parseSearch :: String -> Search
parseSearch "1" = BFS
parseSearch "0" = DFS

toTest :: String -> Test
toTest = (\[x,y] -> (read x, parseSearch y)) . words

solveAll :: (Graph, [(Node, Search)]) -> [[Node]]
solveAll (g, ts) = map solve ts
    where solve (n, s) = search s n
          search DFS = dfs g
          search BFS = bfs g

splitBy :: (a -> Bool) -> [a] -> ([a], [a])
splitBy c ls =
    let (x,y) = span c ls
    in (x, tail y)

toCase :: [String] -> (Case, [String])
toCase ls =
    let (g, ls') = getLinks ls
        (cs, ls'') = splitBy (/= "0 0") ls'
    in ((g, map toTest cs), ls'')

readAll ::  [String] -> Int -> [Case]
readAll ls 0 = []
readAll ls n =
    let (c, ls') = toCase ls
    in c: readAll ls' (n-1)

parseAll :: [String] -> [Case]
parseAll (n:ls) = readAll ls (read n)

parseLink :: String -> (Int, [Int])
parseLink = toTuple . map read . words
    where toTuple (x:n:xs) = (x, take n xs)

getLinks :: [String] -> (Graph, [String])
getLinks (n:ns) =
    let size = read n
        (ls, ps) = splitAt size ns
    in (A.listArray (1, size) . map (snd . parseLink) $ ls, ps)
--    in (M.fromList . map parseLink $ ls, ps)

output :: (Int, [[Int]]) -> String
output (i, c) = unlines $ ("graph " ++ show i) : map (unwords . map show) c

main :: IO ()
main = do
     cases <- parseAll . lines <$> getContents
     mapM_ (putStr . output . second solveAll) $ zip [1..] cases
     return ()
