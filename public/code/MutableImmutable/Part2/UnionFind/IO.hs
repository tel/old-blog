{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module UnionFind.IO (
  UfIO, runUfIO
  ) where

import           AbstractUnionFind.Internal
import           Control.Applicative
import           Data.IORef
import           Mem

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

