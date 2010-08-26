{-# OPTIONS_GHC -O3 #-}
{-# LANGUAGE BangPatterns #-}
import Control.Applicative
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import Control.Monad.Reader
import Control.Arrow
import qualified Data.Set as S

type Node = Int
type Graph = M.Map Node [Node]

metaSearch :: ([Node] -> [Node] -> [Node]) -> Graph -> Node -> [Node]
metaSearch s g start  =  ms S.empty [start]
    where ms !marked [] = []
          ms !marked (n:ns)
              | n `S.member` marked = ms marked ns
              | otherwise  =
                  let children = join . maybeToList $ M.lookup n g
                  in n: ms (n `S.insert` marked) (ns `s` children)

bfs :: Graph -> Node -> [Node]
bfs = metaSearch (++)

dfs :: Graph -> Node -> [Node]
dfs = metaSearch $ flip (++)

data Search = BFS | DFS
            deriving Show

type Test = (Int, Search)
type Case = (Graph, [Test])

parseSearch :: [Char] -> Search
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
splitBy c = do
  xs <- dropWhile c
  ys <- takeWhile c
  return (ys, tail xs)

toCase :: [String] -> (Case, [String])
toCase ls =
    let (g, ls') = getLinks ls
        (cs, ls'') = splitBy (/= "0 0") ls'
    in ((g, map toTest cs), ls'')

readAll ::  [String] -> Int -> [Case]
readAll ls 0 = []
readAll ls n =
    let (c, ls') = toCase ls
    in c:(readAll ls' (n-1))

parseAll :: [String] -> [Case]
parseAll (n:ls) = readAll ls (read n)

parseLink :: String -> (Int, [Int])
parseLink = toTuple . map read . words
    where toTuple (x:n:xs) = (x, take n xs)

getLinks :: [String] -> (Graph, [String])
getLinks (n:ns) = 
    let (ls, ps) = splitAt (read n) ns
    in (M.fromList . map parseLink $ ls, ps)

output :: (Int, [[Int]]) -> String
output (i, c) = unlines $ ("graph " ++ show i) : map (unwords . map show) c

main :: IO ()
main = do
     cases <- parseAll . lines <$> getContents
     mapM_ (putStr . output . second solveAll) $ zip [1..] cases
     return ()
