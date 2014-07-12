{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module UnionFindST where

import           Control.Applicative
import           Control.Monad
import           Data.IORef
import           Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IM

class (Eq (Ref r), Monad r) => Mem r where
  data family Ref r :: *
  type family Val r :: *

  ref   :: Val r -> r (Ref r)
  deref :: Ref r -> r (Val r)
  set   :: Ref r -> Val r -> r ()

alter :: Mem r => (Val r -> Val r) -> Ref r -> r ()
alter f r = do
  v <- deref r
  set r (f v)

--------------------------------------------------------------------------------

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
find n@(Node r) = do
  Node p <- findRec n

  -- PATH COMPRESSION
  -- If we began at the top we don't want to rewrite the parent
  -- but if we're didn't then we cache the root
  unless (r == p) $ alter (\m -> m { parent = Just p }) r

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

test :: UF r Int => r Bool
test = do
  r:rs <- mapM node [1..10]
  mapM_ (link r) rs
  ps <- mapM find rs
  return $ all (eq r) ps
  where
    eq (Node r1) (Node r2) = r1 == r2

--------------------------------------------------------------------------------

newtype UfIO v a =
  UfIO { runUfIO :: IO a }
  deriving ( Functor, Applicative, Monad )

instance Mem (UfIO v) where
  newtype Ref (UfIO v) =
    UfIORef { getUfIORef :: IORef (Node_ (UfIO v) v) } deriving ( Eq )
  type Val (UfIO v) = Node_ (UfIO v) v

  ref   a = UfIO (UfIORef <$> newIORef a)
  deref r = UfIO (readIORef $ getUfIORef r)
  set r v = UfIO (writeIORef (getUfIORef r) v)

--------------------------------------------------------------------------------

newtype State s a = State { runState :: s -> (s, a) }

instance Functor (State s) where
  fmap f (State g) = State $ \s -> let (s', a) = g s in (s', f a)

instance Applicative (State s) where
  pure = return
  (<*>) = ap

instance Monad (State s) where
  return a = State $ \s -> (s, a)
  State g >>= f = State $ \s ->
    let (s', a) = g s in runState (f a) s'

modify :: (s -> s) -> State s ()
modify f = State $ \s -> (f s, ())

gets :: (s -> a) -> State s a
gets f = State (\s -> (s, f s))

put :: s -> State s ()
put s = State (\_ -> (s, ()))

data UfState v =
  UfState { count :: Int
          , mem   :: IntMap (Node_ (UfIM v) v)
          }

newtype UfIM v a =
  UfIM { unUfIM :: State (UfState v) a }
  deriving ( Functor, Applicative, Monad )

runUfIM :: UfIM v a -> a
runUfIM = snd . flip runState ufs0 . unUfIM where
  ufs0 = UfState { count = 0, mem = IM.empty }

fresh :: v -> UfIM v (Ref (UfIM v))
fresh v = UfIM $ do
  c <- gets count
  let n = Node_ { parent = Nothing, rank = 0, value = v }
  modify (\s -> s { count = c + 1, mem = IM.insert c n (mem s) })
  return (UfIMRef c)

instance Mem (UfIM v) where
  newtype Ref (UfIM v) =
    UfIMRef { getUfIMRef :: Int } deriving ( Eq )
  type Val (UfIM v) = Node_ (UfIM v) v

  ref v = UfIM $ do
    c <- gets count
    modify (\s -> s { count = c + 1, mem = IM.insert c v (mem s) })
    return (UfIMRef c)

  deref r = UfIM $ do
    -- WHOA!
    Just v <- gets (IM.lookup (getUfIMRef r) . mem)
    return v

  set r v = UfIM $ do
    modify (\s -> s { mem = IM.insert (getUfIMRef r) v (mem s) })

bug :: Node_ (UfIM ()) ()
bug =
  let
    Node r = runUfIM (node ())
    v = runUfIM (deref r)
  in
   v
