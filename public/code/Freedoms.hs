{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}

module Freedoms where

import           Control.Applicative
import           Control.Category
import           Data.Monoid
import           Data.Profunctor
import           Prelude             hiding (id, (.))

instance Monoid (FMonoid a) where
  mempty  = Fmempty
  mappend = Fmappend

data FMonoid a where
  EmbedMonoid :: a -> FMonoid a
  Fmempty     :: FMonoid a
  Fmappend    :: FMonoid a -> FMonoid a -> FMonoid a
  deriving ( Eq, Show, Functor )

runFMonoid :: Monoid a => FMonoid a -> a
runFMonoid = \case
  EmbedMonoid a -> a
  Fmempty       -> mempty
  Fmappend a b  -> runFMonoid a <> runFMonoid b

--------------------------------------------------------------------------------

data FCategory k a b where
  EmbedCategory :: k a b -> FCategory k a b
  Fid           :: FCategory k a a
  Fcompose      :: FCategory k b c -> FCategory k a b -> FCategory k a c

instance Category (FCategory k) where
  id  = Fid
  (.) = Fcompose

runFCategory :: Category k => FCategory k a b -> k a b
runFCategory = \case
  EmbedCategory k -> k
  Fid             -> id
  Fcompose f g    -> runFCategory f . runFCategory g

--------------------------------------------------------------------------------
-- Coyoneda formulation

data FFunctor f a where
  EmbedFunctor :: f a -> FFunctor f a
  Ffmap        :: (x -> a) -> FFunctor f x -> FFunctor f a

instance Functor (FFunctor f) where
  fmap = Ffmap

runFFunctor :: Functor f => FFunctor f a -> f a
runFFunctor = \case
  EmbedFunctor fa -> fa

-- (1) We can eliminate EmbedFunctor, obviously. But must we?
-- (2) What is Yoneda?

--------------------------------------------------------------------------------

data FApplicative f a where
  EmbedApplicative :: f a -> FApplicative f a
  Fpure            :: a -> FApplicative f a
  Fapply           :: FApplicative f (a -> b) -> FApplicative f a -> FApplicative f b

instance Functor f => Functor (FApplicative f) where
  fmap f = \case
    EmbedApplicative fa -> EmbedApplicative (fmap f fa)
    Fpure a             -> Fpure (f a)
    Fapply fun arg      -> Fapply (fmap (f .) fun) arg

instance Functor f => Applicative (FApplicative f) where
  pure  = Fpure
  (<*>) = Fapply

runFApplicative :: Applicative f => FApplicative f a -> f a
runFApplicative = \case
  EmbedApplicative fa -> fa
  Fpure a             -> pure a
  Fapply fun arg      -> runFApplicative fun <*> runFApplicative arg

--------------------------------------------------------------------------------

data FProfunctor k a b where
  EmbedProfunctor :: k a b -> FProfunctor k a b
  Fdimap          :: (a -> b) -> (c -> d) -> FProfunctor k b c -> FProfunctor k a d

instance Profunctor (FProfunctor k) where
  dimap = Fdimap

runFProfunctor :: Profunctor k => FProfunctor k a b -> k a b
runFProfunctor = \case
  EmbedProfunctor k -> k
  Fdimap f g k      -> dimap f g (runFProfunctor k)

--------------------------------------------------------------------------------

data FStrong k a b where
  EmbedStrong :: k a b -> FStrong k a b
  Ffirst'     :: FStrong k a b -> FStrong k (a, c) (b, c)
  Fsecond'    :: FStrong k a b -> FStrong k (c, a) (c, b)

instance Profunctor k => Profunctor (FStrong k) where
  -- (a -> b) -> (c -> d) -> FStrong k b c -> FStrong k a d
  dimap f g k = case k of
    EmbedStrong k -> EmbedStrong (dimap f g k)
    Ffirst' k     -> _first'
    Fsecond' k    -> _second'
