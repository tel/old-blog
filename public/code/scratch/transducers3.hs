{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

module Transducers where

import Control.Arrow
import Control.Category
import Control.Comonad
import Control.Monad
import Data.Profunctor
import Prelude hiding (id, (.))

data Foldf s i o =
  Foldf {
      state  :: s
    , update :: i -> s -> s
    , output :: s -> o
    }
  deriving Functor

instance Profunctor (Foldf s) where
  rmap = fmap
  lmap f it = it { update = \i s -> update it (f i) s }

data Fold i o where
  Fold :: Foldf s i o -> Fold i o

newtype (~>) a b =
  Transducer (forall r . Fold b r ->  -> Fold a r)

-- extend :: Monad m => (a -> m b) -> (m a -> m b)
-- extend = (=<<)

-- instance Category (~>) where
--   id = Transducer id
--   Transducer g . Transducer f = Transducer (f . g)

-- tmap :: (a -> b) -> (a ~> b)
-- tmap f = Transducer $ \brr a r -> brr (f a) r

-- tfilt :: (a -> Bool) -> (a ~> a)
-- tfilt p = Transducer $ \brr a r -> if p a then brr a r else return r

-- tflatMap :: (a -> [b]) -> (a ~> b)
-- tflatMap f = Transducer $ \brr a r -> foldr (\a -> extend (brr a)) (Right r) (f a)

-- tseq :: (a ~> b) -> [a] -> [b]
-- tseq (Transducer f) = either id id . spin where
--   spin []     = Right []
--   spin (a:as) = spin as >>= f (\x xs -> Right (x:xs)) a
