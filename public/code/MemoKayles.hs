
import           Data.IntSet (IntSet)
import qualified Data.IntSet as Is

type Board = IntSet
data Trie  = Trie Bool Trie Trie

mkBoard :: [Int] -> Board
mkBoard = Is.fromList

win :: Board -> Bool
win b = untrie (trie win') b where
  win' :: Board -> Bool
  win' b = case Is.size b of
    0 -> False
    1 -> True  -- singleton games are winnable
    _ -> (not . all win . moves) b

trie :: (Board -> Bool) -> Trie
trie f = trie' 1 Is.empty where
  trie' n b =
    Trie (f b)
         (trie' (succ n) (Is.insert n b))
         (trie' (succ n) b)

untrie :: Trie -> (Board -> Bool)
untrie t s = untrie' t 1 (Is.toAscList s) where
  untrie' (Trie h _ _) n []     = h
  untrie' (Trie h t f) n keys@(k:ks)
    | n == k = untrie' t (n+1) ks
    | n < k = untrie' f (n+1) keys
    | n > k = error "impossible"

data Move a =
  Zero | One a | Two a a

moves :: Board -> [Board]
moves b = do
  pivot <- Is.toAscList b
  let b' = Is.delete pivot b
  mv    <- moves1 pivot
  return $ case mv of
    Zero  -> b'
    One j -> zap j b'
    Two i j | i == j    -> b'
            | otherwise -> (zap i . zap j) b'

  where

    zap :: Int -> IntSet -> IntSet
    zap i s | Is.member i s = Is.delete i s
            | otherwise     = Is.insert i s

    -- | Compute the moves possible using a single stack
    moves1 :: Int -> [Move Int]
    moves1 1 = [Zero]
    moves1 2 = [One 1, Zero]
    moves1 3 = [One 2, Two 1 1, One 1]
    moves1 n = twoPart (n-1) ++ twoPart (n-2)

    -- partitions of numbers into two pieces
    twoPart :: Int -> [Move Int]
    twoPart n =
      map (\m ->
            if m == 0
              then One (n-m)
              else Two m (n-m))
          [0.. n `div` 2]


main :: IO ()
main = do
  print (win (mkBoard [80,1]))
