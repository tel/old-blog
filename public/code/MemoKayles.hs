
module MemoKayles where

type Board = [Bool]

data Trie = Trie !Bool Trie Trie

moves :: Board -> [Board]
moves = undefined

loss :: Board -> Bool
loss = not . win

win :: Board -> Bool
win = (untrie . trie) win' where
  win' :: Board -> Bool
  win' [ ] = False
  win' [_] = True   -- singleton games are winnable
  win' b   = all loss (moves b)

untrie :: Trie -> (Board -> Bool)
untrie (Trie h _ _) []        = h
untrie (Trie _ t _) (True :n) = untrie t n
untrie (Trie _ _ f) (False:n) = untrie f n

trie :: (Board -> Bool) -> Trie
trie f = go [] where
  go b = Trie (f b)
              (go (True  : b))
              (go (False : b))
