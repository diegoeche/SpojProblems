{-# OPTIONS_GHC -O3 #-}
{-# LANGUAGE BangPatterns #-}
import Control.Applicative
-- import qualified Data.Map as M
-- import Data.Maybe (maybeToList)
-- import Data.List (span)
-- import Control.Monad.Reader
-- import Control.Monad (join)
import Control.Arrow (second)
-- import qualified Data.Set as S
import qualified Data.Array as A
import Data.Array.ST
import Control.Monad.ST

type Node = Int
-- type Graph = M.Map Node [Node]
type Graph = A.Array Node [Node]

data CL a = Head [a] | (CL a) :-: (CL a)
                deriving Show

popCL :: CL t -> (t, CL t)
popCL (Head []) = error "Getting head"
popCL (Head (x:xs)) = (x, Head xs)
popCL (Head [] :-: conc) = popCL conc
popCL (c1 :-: c2) =
    let (h, c1') = popCL c1
    in (h, c1' :-: c2)

cons :: CL t -> CL t -> CL t
cons (Head []) c = c
cons c (Head []) = c
cons c1 c2 = c1 :-: c2

-- metaSearch2 :: ([Node] -> [Node] -> [Node]) -> Graph -> Node -> [Node]
-- metaSearch2 s g start  =  ms S.empty [start]
--     where ms !marked [] = []
--           ms !marked (n:ns)
--               | n `S.member` marked = ms marked ns
--               | otherwise  =
--                   let children = join . maybeToList $ M.lookup n g
--                   in n: ms (n `S.insert` marked) (ns `s` children)

metaSearch :: (CL Node -> CL Node -> CL Node) -> Graph -> Node -> [Node]
metaSearch s g start  = runST $ ms (Head [start])
    where ms vs = do
            let size = snd $ A.bounds g
            marked <- newListArray (0, size-1) $ repeat False :: ST s (STUArray s Int Bool)
            let loop (Head []) = return []
                loop cl = do
                    let (n, ns) = popCL cl
                    isMarked <- readArray marked (n-1)
                    if isMarked then loop ns
                        else do
                            writeArray marked (n-1) True
--                            let children = join . maybeToList $ M.lookup n g
                            let children = g A.! n
                            (n:) <$> loop (ns `s` (Head children))
--             let loop [] = return []
--                 loop (n:ns) = do
--                     isMarked <- readArray marked (n-1)
--                     if isMarked then loop ns
--                         else do
--                             writeArray marked (n-1) True
-- --                            let children = join . maybeToList $ M.lookup n g
--                             let children = g A.! n
--                             (n:) <$> loop (ns `s` children)
            loop vs

-- bfs :: Graph -> Node -> [Node]
bfs :: Graph -> Node -> [Node]
bfs = metaSearch cons

dfs :: Graph -> Node -> [Node]
dfs = metaSearch $ flip cons

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

graph = ["6", "1 2 3 4", "2 2 3 6", "3 2 1 2", "4 1 1", "5 0", "6 1 2", "5 1", "1 0", "1 0", "0 0"]


main :: IO ()
main = do
     cases <- parseAll . lines <$> getContents
     mapM_ (putStr . output . second solveAll) $ zip [1..] cases
     return ()
