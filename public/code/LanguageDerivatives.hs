{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}

-- http://matt.might.net/papers/might2011derivatives.pdf
module LangDerive where

import           Prelude hiding ((*), (+))
import qualified Prelude as P

infixl 6 +
infixl 7 *
class StarSNR a where
  zero :: a
  one  :: a
  (+)  :: a -> a -> a
  (*)  :: a -> a -> a
  star :: a -> a

instance StarSNR Bool where
  zero   = False
  one    = True
  a + b  = a || b
  a * b  = a && b
  star _ = True

liftBool :: StarSNR a => Bool -> a
liftBool b = if b then one else zero

data FStarSNR a
  = Zero
  | One
  | Plus (FStarSNR a) (FStarSNR a)
  | Mult (FStarSNR a) (FStarSNR a)
  | Star (FStarSNR a)
  | Embed a
  deriving (Show, Functor)

e :: a -> FStarSNR a
e = Embed

instance StarSNR (FStarSNR a) where
  zero = Zero
  one  = One
  a + b = case (a, b) of
    (Zero, s)         -> s
    (s, Zero)         -> s
    _                 -> Plus a b
  a * b = case (a, b) of
    (Zero, _)       -> Zero
    (_, Zero)       -> Zero
    (One, s)        -> s
    (s, One)        -> s
    (Plus l1 l2, r) -> Plus (l1 * r) (l2 * r)
    _               -> Mult a b
  star = Star

-- This is the equivalent of foldMap from Foldable
lowerMap :: StarSNR b => (a -> b) -> FStarSNR a -> b
lowerMap f = z where
  z x = case x of
    Embed a  -> f a
    Zero     -> zero
    One      -> one
    Plus a b -> z a + z b
    Mult a b -> z a * z b
    Star r   -> star (z r)

deriv :: (a -> FStarSNR a) -> FStarSNR a -> FStarSNR a
deriv f = z where
  z l = case l of
    Zero     -> zero
    One      -> zero
    Plus a b -> deriv f a + deriv f b
    Mult a b -> deriv f a * b + trivial a * deriv f b
    Embed c  -> f c

type Lang = FStarSNR Char

derivLang :: Eq a => a -> FStarSNR a -> FStarSNR a
derivLang c0 = deriv (\c -> liftBool (c == c0))

-- exL :: Lang
exL = exL * (e 'a' + e 'b') + one

trivial :: StarSNR c => FStarSNR a -> c
trivial = lowerMap (const zero)

match :: Eq a => [a] -> FStarSNR a -> Bool
match []    l = trivial l
match (c:w) l = match w (derivLang c l)
