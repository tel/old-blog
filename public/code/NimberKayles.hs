{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State.Strict
import           Data.Bits
import           Data.Hashable
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HM
import           Data.List
import           GHC.Exts
import           GHC.Integer.Logarithms
import           GHC.Prim


-- |The type of finite nimbers.
newtype Nimber = Nimber Integer deriving (Eq, Ord)

intLog2 :: Int# -> Int
intLog2 i# = I# (wordLog2# (int2Word# i#))

nimSize :: Nimber -> Int
nimSize (Nimber a) = bit (intLog2 (integerLog2# a))

nimSplit :: Int -> Nimber -> (Nimber, Nimber)
nimSplit k = \(Nimber a) -> (Nimber (a `shiftR` k), Nimber (a .&. mask)) where
  mask = complement (-1 `shiftL` k)

nimJoin :: Int -> (Nimber, Nimber) -> Nimber
nimJoin k (Nimber a1, Nimber a0) = Nimber ((a1 `shiftL` k) .|. a0)

nimBit :: Int -> Nimber
nimBit i = Nimber (bit i)

nimSqr :: Nimber -> Nimber
nimSqr 0 = 0
nimSqr 1 = 1
nimSqr a = nimJoin k (p1, p0 + p1 * nimBit (k - 1)) where
  k = nimSize a
  (a1, a0) = nimSplit k a
  p0 = nimSqr a0
  p1 = nimSqr a1

instance Show Nimber where
  showsPrec d (Nimber a) = showParen (d > 7) $ showChar '*' . showsPrec 8 a

instance Read Nimber where
  readsPrec d = readParen (d > 7) $ \s -> case s of
    '*' : s1 -> [(fromInteger a, s2) | (a, s2) <- readsPrec 8 s1]
    _ -> []

instance Enum Nimber where
  pred (Nimber a) = fromInteger (pred a)
  succ (Nimber a) = fromInteger (succ a)
  toEnum a = fromInteger (toEnum a)
  fromEnum (Nimber a) = fromEnum a

instance Num Nimber where
  Nimber a + Nimber b = Nimber (a `xor` b)

  0 * _ = 0
  _ * 0 = 0
  1 * a = a
  a * 1 = a
  a * b | a == b = nimSqr a
  a * b = nimJoin k (p0 + (a0 + a1) * (b0 + b1), p0 + p1 * nimBit (k - 1)) where
    k = max (nimSize a) (nimSize b)
    split = nimSplit k
    (a1, a0) = split a
    (b1, b0) = split b
    p0 = a0 * b0
    p1 = a1 * b1

  (-) = (+)
  negate = id
  abs = id
  signum 0 = 0
  signum _ = 1
  fromInteger = Nimber


type Board = [Int]
type Memo  = HashMap Board Bool

buildBoard :: [Int] -> Board
buildBoard = sort

--check0 :: Board -> Bool
check0 b = runState (check b) HM.empty

check :: Board -> State Memo Bool
check b = do
  m <- get
  let b' = oddOnly b
  case HM.lookup b' m of
    Just v  -> return v
    Nothing -> do
      res <- compute b'
      modify (HM.insert b' res)
      return res

oddOnly :: (Hashable a, Eq a) => [a] -> [a]
oddOnly = HM.keys . HM.filter odd . freq

ones :: Board -> Bool
ones = all (\x -> x == 1)

freq :: (Eq k, Hashable k) => [k] -> HashMap k Int
freq = foldr (\k -> HM.insertWith (+) k 1) HM.empty

compute :: Board -> State Memo Bool
compute []  = return False
compute [_] = return True -- 1-stacks always win
compute b = do
  opponent <- mapM check (moves b)
  return (any not opponent)

moves :: [Int] -> [[Int]]
moves sets = do
  (left, x, right) <- ctxs sets
  part             <- moves1 x
  return $ (sort $ part ++ reverse left) ++ right

  where

    ctxs :: [a] -> [([a], a, [a])]
    ctxs [] = []
    ctxs (a:as) = run ([], a, as) where
      run x@(fore, here, [])     = [x]
      run x@(fore, here, h:hind) = x : run (here:fore, h, hind)

    moves1 :: Int -> [Board]
    moves1 1 = [[]]
    moves1 2 = [[1],[]]
    moves1 3 = [[2], [1,1], [1]]
    moves1 n = do
      (i,j) <- twoPartitions (n-1) ++ twoPartitions (n-2)
      return $ if i == 0 then [j] else [i,j]

    twoPartitions :: Int -> [(Int, Int)]
    twoPartitions n = [ (m, n-m) | m <- [0.. n `div` 2] ]

parse :: String -> [Int]
parse = filter (> 0) . map length . groupBy (=='X') ([], []) where
  groupBy p (working, total) []     = reverse (working : total)
  groupBy p (working, total) (a:as)
    | not (p a)   = groupBy p (a:working, total)  as
    | otherwise = groupBy p ([], working:total) as

main :: IO ()
main = do
  n <- readLn
  cases <- replicateM n (getLine >> getLine)
  let st  = mapM (check . buildBoard . parse) cases
      res = evalState st HM.empty
  forM_ res $ \r ->
    if r then putStrLn "WIN" else putStrLn "LOSE"
