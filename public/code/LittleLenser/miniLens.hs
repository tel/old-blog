{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module MiniLens where

import Control.Applicative
import Data.Functor.Constant
import Data.Monoid

class Profunctor p where
  dimap :: (a' -> a) -> (b -> b') -> (p a b -> p a' b')
  lmap :: (a' -> a) -> (p a b -> p a' b)
  lmap f = dimap f id
  rmap :: (b -> b') -> (p a b -> p a b')
  rmap f = dimap id f

instance Profunctor (->) where
  dimap f g h = g . h . f

class Profunctor p => Strong p where
  first  :: p a b -> p (a, c) (b, c)
  second :: p a b -> p (c, a) (c, b)

instance Strong (->) where
  first  f (a, c) = (f a, c)
  second f (c, a) = (c, f a)

diag :: a -> (a, a)
diag a = (a, a)

class Profunctor p => Choice p where
  left  :: p a b -> p (Either a c) (Either b c)
  right :: p a b -> p (Either c a) (Either c b)

instance Choice (->) where
  left  f = either (Left . f) Right
  right f = either Left (Right . f)

type Optic p s t a b = p a b -> p s t
type Iso     s t a b = forall p . Profunctor p => Optic p s t a b
type Lens    s t a b = forall p . Strong p     => Optic p s t a b
type Prism   s t a b = forall p . Choice p     => Optic p s t a b

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso = dimap

-- | Reified Iso. Known as Exchange in `lens`.
data RIso a b s t = RIso (s -> a) (b -> t)

instance Profunctor (RIso a b) where
  dimap f g (RIso sa bt) = RIso (sa . f) (g . bt)

from :: Iso s t a b -> Iso b a t s
from p = case p (RIso id id) of
  RIso f g -> iso g f

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens view set = dimap (second view . diag) (uncurry set) . second

newtype P f a b = P { runP :: a -> f b }

instance Functor f => Profunctor (P f) where
  dimap f g (P x) = P $ \a -> fmap g (x (f a))

instance Functor f => Strong (P f) where
  first  (P x) = P $ \(a, c) -> fmap (,c) (x a)
  second (P x) = P $ \(c, a) -> fmap (c,) (x a)

instance Applicative f => Choice (P f) where
  left (P x) = P $ \case
    Left  a -> fmap Left (x a)
    Right c -> pure (Right c)
  right (P x) = P $ \case
    Right a -> fmap Right (x a)
    Left  c -> pure (Left c)

_1 :: Lens (a, b) (c, b) a c
_1 = lens fst (\(a, b) c -> (c, b))

_2 :: Lens (a, b) (a, c) b c
_2 = lens snd (\(a, b) c -> (a, c))

_Left :: Prism (Either a b) (Either c b) a c
_Left = prism Left $ \case
  Right a -> Left (Right a)
  Left  a -> Right a

_Right :: Prism (Either a b) (Either a c) b c
_Right = prism Right $ \case
  Left  a -> Left (Left a)
  Right a -> Right a

view :: Optic (P (Constant a)) s t a b -> (s -> a)
view l = getConstant . runP (l (P Constant))

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism build break = dimap break (either id build) . right

prism' :: (b -> s) -> (s -> Maybe a) -> Prism s s a b
prism' build break' = prism build (\s -> maybe (Left s) Right . break' $ s)

preview :: Optic (P (Constant (First a))) s t a b -> (s -> Maybe a)
preview l = getFirst . getConstant . runP (l (P (Constant . First . Just)))
