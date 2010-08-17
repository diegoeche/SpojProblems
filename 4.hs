import Control.Monad.Reader
import Data.Function
import Test.QuickCheck
import Control.Monad.State

newtype Expr = Expr String
        deriving (Show, Eq)

instance Arbitrary Expr where
  arbitrary = sized expr'
    where expr' 0 = liftM Expr (oneof [return [x] | x <- ['a'..'z']])
	  expr' n | n > 0 = oneof operators
  	    where subtree = expr' (n `div` 2)
                  oper s = do
                       Expr x <- subtree
                       Expr y <- subtree
                       return . Expr $ x ++ s:[] ++ y
                  operators = map oper "+-/*^"
  shrink x = [x]

prop :: Expr -> Bool
prop (Expr x) = toRPN x == toRPN' x
prop' (Expr x) = toRPN'' x == toRPN' x


pqInsert dx opx []  = [opx,dx]
pqInsert dx opx [x] | not $ isOp x = [opx,dx,x]
                    | undefined  = error "error 2"
pqInsert dx opx (opy:ys) | ((<=) `on` precedence) opx opy = opx:dx:opy:ys
                         | otherwise = opy:pqInsert dx opx ys

toRPN [] = []
toRPN [x] = [x]
toRPN (x:x':x'':xs)  = reverse . loop xs $ pqInsert x'' x' [x]
      where loop [] = id
            loop (y:y':ys) = loop ys . pqInsert y' y

isOp :: Char -> Bool
isOp = (`elem` "+-/*^")

precedence :: (Num t) => Char -> t
precedence '^' = 5
precedence '/' = 4
precedence '*' = 3
precedence '-' = 2
precedence '+' = 1
precedence _   = 10

toRPN' = rpn [] []
  where rpn accum stack []       = reverse accum ++ stack
        rpn accum stack ('(':xs) = rpn accum ('(':stack) xs
        rpn accum stack (')':is) = multiPop $ break (== '(') stack
                where multiPop (xs,_:ys) = rpn (reverse xs ++ accum) ys is
                      multiPop (xs,[])   = rpn (reverse xs ++ accum) [] is
        rpn accum stack input@(x:xs)
               | not $ isOp x = rpn (x:accum) stack xs
               | otherwise  =
                        case stack of
                        []    -> rpn accum [x] xs
                        '(':_ -> pushX
                        op:ops  -> if ((>=) `on` precedence) op x
                                  then rpn (op:accum) ops input
                                  else pushX
                  where pushX = rpn accum (x:stack) xs

type Stack a = [a]

type RPNState = (Stack Char, String)

accum :: State RPNState String
accum = do
      (_,accum) <- get
      return accum

stack :: State RPNState (Stack Char)
stack = fst `fmap` get

push x = do
     (s,accum) <- get
     put (x:s, accum)
     return ()

modifyAccum f = do
     (s,accum) <- get
     put (s, f accum)
     return ()

modifyStack f = do
     (s,accum) <- get
     put (f s, accum)
     return ()

toRPN'' :: [Char] -> [Char]
toRPN'' input = fst $ runState (rpn input) ([], [])
         where rpn [] = liftM2 (++) (reverse `liftM` accum) stack
               rpn ('(':xs) = push '(' >> rpn xs
               rpn (')':is) = do
                   s <- stack
                   let (xs, newStack) = break (== '(') s
                   modifyAccum (reverse xs ++)
                   case newStack of
                    (_:ys) -> modifyStack (const ys) >> rpn is
                    []     -> modifyStack (const []) >> rpn is
               rpn input@(x:xs)
                   | not $ isOp x = modifyAccum (x:) >> rpn xs
                   | otherwise  = do
                     s <- stack
                     case s of
                      []     -> push x >> rpn xs
                      '(':_  -> push x >> rpn xs
                      op:ops -> if ((>=) `on` precedence) op x
                               then do modifyAccum (op:)
                                       modifyStack (const ops)
                                       rpn input
                               else push x >> rpn xs


        -- rpn accum stack input@(x:xs)
        --        | not $ isOp x = rpn (x:accum) stack xs
        --        | otherwise  =
        --                 case stack of
        --                 []    -> rpn accum [x] xs
        --                 '(':_ -> pushX
        --                 op:ops  -> if ((>=) `on` precedence) op x
        --                           then rpn (op:accum) ops input
        --                           else pushX
        --           where pushX = rpn accum (x:stack) xs



main :: IO ()
main = do
     x:xs <- liftM lines getContents
     mapM_ (putStrLn . toRPN') xs