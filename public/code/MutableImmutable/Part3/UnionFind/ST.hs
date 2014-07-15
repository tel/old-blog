{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module UnionFind.ST (
  UfST, runUfST
  ) where

import AbstractUnionFind.Internal
import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.STRef
import Mem

newtype UfST s v a =
  UfST { unUfST :: ST s a }
  deriving ( Functor, Applicative, Monad )

runUfST :: (forall s. UfST s v a) -> a
runUfST comp = runST (unUfST comp)

instance Mem (UfST s v) where
  newtype Ref (UfST s v) = UfSTRef { getUfSTRef :: STRef s (Val (UfST s v)) } deriving ( Eq )
  type    Val (UfST s v) = Node_ (UfST s v) v

  ref   a = UfST (UfSTRef <$> newSTRef a)
  deref r = UfST (readSTRef $ getUfSTRef r)
  set r v = UfST (writeSTRef (getUfSTRef r) v)

exPure = runUfST c where
  c :: UF r () => r Bool
  c = do
    n1 <- node ()
    n2 <- node ()
    link n1 n2
    connected n1 n2
