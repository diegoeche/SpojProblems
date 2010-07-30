{-# LANGUAGE ForeignFunctionInterface #-}
-- or -fglasgow-exts
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -O2 #-}
import System.Environment (getArgs)
import System.Posix.Process (executeFile)
import Foreign.C.Types (CInt)
import Foreign.C.String (CString, peekCString)
import Foreign (peek, alloca, peekElemOff, Ptr)
import Control.Applicative
import qualified Data.ByteString.Char8 as B
import Data.Array.ST
import Data.Array.Base (unsafeRead, unsafeWrite, unsafeAt)
import Control.Monad.ST
import Data.ByteString.Unsafe (unsafeIndex)
import Data.ByteString.Internal (w2c)

-- addOne :: B.ByteString -> B.ByteString
-- addOne xs = B.pack list
--   where
--      !l   = B.length xs
--      {-# INLINE charAt #-}
--      charAt i = w2c (unsafeIndex xs i)
--      !uar = runSTUArray $ do
--         arr <- newArray_ (0,l-1)
--         let fill i
--               | i < l      = unsafeWrite arr i (charAt i) >> fill (i+1)
--               | otherwise = return ()
--             loop (-1) = unsafeWrite arr 0 'a'
--             loop i = do
--                v <- unsafeRead arr i
--                case v of
--                  '9' -> unsafeWrite arr i '0' >> loop (i-1)
--                  _   -> unsafeWrite arr i (succ v)
--         fill 0
--         loop (l-1)
--         return arr
--      list = case uar `unsafeAt` 0 of
--               'a' -> '1':replicate l '0'
--               c   -> c : [uar `unsafeAt` i | i <- [1 .. l-1]]


addOne :: B.ByteString -> B.ByteString
addOne xs = B.pack list
  where
     !l   = B.length xs
     !uar = runSTUArray $ do
        arr <- newListArray (0,l-1) $ B.unpack xs
        let loop (-1) = unsafeWrite arr 0 'a'
            loop i = do
               v <- unsafeRead arr i
               case v of
                 '9' -> unsafeWrite arr i '0' >> loop (i-1)
                 _   -> unsafeWrite arr i (succ v)
        loop (l-1)
        return arr
     list = case uar `unsafeAt` 0 of
              'a' -> '1':replicate l '0'
              c   -> c : [uar `unsafeAt` i | i <- [1 .. l-1]]

-- addOne :: B.ByteString -> B.ByteString
-- addOne xs =
--         let fix ('a':rest) = '1':'0':rest
--             fix x = x
--             l  = B.length xs
--         in  runST $
--           do arr <- newListArray (1, l) (B.unpack xs) :: ST s (STUArray s Int Char)
--              -- arr <- newArray_ (1, l)  :: ST s (STArray s Int Char)
--              -- forM_ [1..l] (\idx -> writeArray arr idx (xs `B.index` (idx - 1)))
--              let loop 0   = writeArray arr 1 'a'
--                  loop idx = do
--                     value <- readArray arr idx
--                     case value of
--                      '9' -> writeArray arr idx '0' >> loop (idx - 1)
--                      x   -> writeArray arr idx (succ x)
--              loop l
--              B.pack . fix <$> getElems arr

-- foo :: B.ByteString -> B.ByteString
-- foo xs = let l = B.length xs in B.pack . runST $ do
--     arr <- newArray_ (1, 2 * l)  :: ST s (STArray s Int Char)
--     forM_ [1..l] (\idx -> writeArray arr idx (xs `B.index` (idx - 1)))
--     forM_ [l+1..2 * l] (\idx -> writeArray arr idx '0')
--     let fix ('a':rest) = '1':'0':rest
--         fix x = x
--         loop 0   = writeArray arr 1 'a'
--         loop idx = do
--              value <- readArray arr idx
--              case value of
--               '9' -> writeArray arr idx '0' >> loop (idx - 1)
--               x   -> writeArray arr idx (succ x)
--     loop l
--     fix <$> getElems arr

-- addOne xs =
--         let fix ('a':rest) = '1':'0':rest
--             fix x = x
--             l  = length xs
--         in  fix . runST $
--           do arr <- newListArray (1, l) xs :: ST s (STUArray s Int Char)
--              -- arr <- newArray_ (1, l)  :: ST s (STArray s Int Char)
--              -- forM_ [1..l] (\idx -> writeArray arr idx (xs `B.index` (idx - 1)))
--              let loop 0   = writeArray arr 1 'a'
--                  loop idx = do
--                     value <- readArray arr idx
--                     case value of
--                      '9' -> writeArray arr idx '0' >> loop (idx - 1)
--                      x   -> writeArray arr idx (succ x)
--              loop l
--              getElems arr


-- biggerOrEquals :: B.ByteString -> B.ByteString -> Bool
-- biggerOrEquals x y =
--          let (lx, ly) = (B.length x, B.length y)
--          in case compare lx ly of
--             GT -> True
--             LT -> False
--             EQ -> case dropWhile (==EQ) $ B.zipWith compare x y of
--                   [] -> True
--                   (GT:_) -> True
--                   _  -> False

-- addOne :: B.ByteString -> B.ByteString
-- addOne = B.reverse . inc . B.reverse
--        where inc x | B.length x == 0 = B.singleton '1'
--                    | B.head x == '9' =  '0' `B.cons` (inc $ B.tail x)
--                    | otherwise = (succ $ B.head x) `B.cons` (B.tail x)

-- addOne :: B.ByteString -> B.ByteString
-- addOne = B.reverse . B.pack . inc . B.unpack . B.reverse
--        where inc x | length x == 0 = ['1']
--                    | head x == '9' =  '0' : (inc $ tail x)
--                    | otherwise = (succ $ head x) : (tail x)

solve :: B.ByteString -> B.ByteString
solve x | B.length x == 0 = error "empty list"
        | B.length x == 1 = x
        | otherwise =
      let half = B.length x `div` 2
          (l, rs) = B.splitAt half x
          rl     = B.reverse l
      in if even $ B.length x then
--            if rl `biggerOrEquals` rs
            if rl >= rs
            then l `B.append` rl
            else solve $ (addOne l) `B.append` (B.replicate half '0')
                 -- solve $ foo l
         else let (fr,r) = (B.head rs, B.tail rs)
              in if rl >= r
                 then l `B.append` (fr `B.cons` rl)
                 else solve $ (addOne (l `B.snoc` fr)) `B.append` (B.replicate half '0')


output :: B.ByteString -> IO ()
output = B.putStrLn
--output = putStrLn

realMain :: IO ()
realMain = do
     n:ls <- B.lines <$> B.getContents
--     n:ls <- lines <$> getContents
--     B.interact (B.unlines . map (solve . addOne) . tail . (B.lines))
     mapM_ (output . solve . addOne) $ take (read $ B.unpack n) ls
--     mapM_ (output . solve . addOne) $ take (read  n) ls
     return ()



main = do
     realMain
  -- flags    <- getArgs
  -- progname <- getFullProgName
  -- if null flags
  --   then
  --     -- Supply an "argument" so that flags will not be null.
  --     -- RTS option -A100m will increase the allocation area size
  --     -- to 100 megabytes.
  --     executeFile progname False ["r","+RTS","-A100m"] Nothing
  --   else
  --     realMain

-- Now the trickier part: getProgName in GHC does not return the
-- full path, for "portability" reasons.  SPOJ does not run
-- programs from the current directory.  That means we need to
-- find the full path to the program some other way.

foreign import ccall unsafe "getProgArgv"
  getProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()

-- As it turns out, the C function which getProgName uses actually
-- does return the full path.  But then getProgName cuts it out
-- before returning it.  This is a version of getProgName which
-- leaves the full path intact.

getFullProgName :: IO String
getFullProgName =
  alloca $ \ p_argc ->
  alloca $ \ p_argv -> do
    getProgArgv p_argc p_argv
    argv <- peek p_argv
    s    <- peekElemOff argv 0 >>= peekCString
    return s