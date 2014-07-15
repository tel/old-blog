{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module UnionFind.IntMap (
  UfIntMap, runUfIntMap
  ) where

import           AbstractUnionFind.Internal
import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Data.IORef
import           Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IM
import           Mem

data Uf s v =
  Uf { count :: Int
     , mem   :: IntMap (Node_ (UfIntMap s v) v)
     }

uf0 :: Uf s v
uf0 = Uf { count = 0, mem = IM.empty }

newtype UfIntMap s v a =
  UfIntMap { unUfIntMap :: State (Uf s v) a }
  deriving ( Functor, Applicative, Monad )

runUfIntMap :: UfIntMap s v a -> a
runUfIntMap = flip evalState uf0 . unUfIntMap where

instance Mem (UfIntMap s v) where
  newtype Ref (UfIntMap s v) = UfIntMapRef { getId :: Int } deriving ( Eq )
  type    Val (UfIntMap s v) = Node_ (UfIntMap s v) v

  ref v = UfIntMap $ do
    c <- gets count
    modify (\s -> s { count = c + 1, mem = IM.insert c v (mem s) })
    return (UfIntMapRef c)

  deref r = UfIntMap $ do
    -- WHOA!
    Just v <- gets (IM.lookup (getId r) . mem)
    return v

  set r v = UfIntMap $ do
    modify (\s -> s { mem = IM.insert (getId r) v (mem s) })

exPure :: Bool
exPure = runUfIntMap computation where
  computation = do
    n1 <- node 1
    n2 <- node 2
    link n1 n2
    connected n1 n2
