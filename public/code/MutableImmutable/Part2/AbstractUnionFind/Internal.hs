{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies    #-}

module AbstractUnionFind.Internal where

import Control.Monad
import Mem

data Node_ r a =
  Node_ { parent :: Maybe (Ref r)
        , rank   :: Int
        , value  :: a
        }

newtype Node r = Node (Ref r)

type UF r a = (Mem r, Val r ~ Node_ r a)

node :: UF r a => a -> r (Node r)
node a = do
  r <- ref (Node_ { parent = Nothing, rank = 0, value = a })
  return (Node r)

-- performs path compression
find :: UF r a => Node r -> r (Node r)
find (Node r) = do
  Node p <- findRec (Node r)

  -- PATH COMPRESSION
  -- If we began at the top we don't want to rewrite the parent
  -- but if we're didn't then we cache the root
  unless (r == p) $ alter (\n -> n { parent = Just p }) r

  return (Node p)

  where
    -- | Recursively jump up `parent` links until we're
    --   at a root node
    findRec :: UF r a => Node r -> r (Node r)
    findRec (Node r) = do
      n <- deref r
      case parent n of
        Nothing -> return (Node r)
        Just p  -> find (Node p)

link :: UF r a => Node r -> Node r -> r ()
link n1 n2 = do
  Node p1 <- find n1
  Node p2 <- find n2
  unless (p1 == p2) (adopt p1 p2)
  where
    adopt x y = do
      nx <- deref x
      ny <- deref y
      case compare (rank nx) (rank ny) of
        EQ -> do set x (nx { rank   = rank nx + 1 })
                 set y (ny { parent = Just x      })
        LT -> set x (nx { parent = Just y })
        GT -> set y (ny { parent = Just x })

connected :: UF r a => Node r -> Node r -> r Bool
connected n1 n2 = do
  Node p1 <- find n1
  Node p2 <- find n2
  return (p1 == p2)
