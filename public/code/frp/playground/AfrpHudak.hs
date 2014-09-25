{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module AfrpHudak where

import Control.Arrow
import Control.Category
import Data.Profunctor
import Prelude hiding ((.), id)

type Time = Double

type Signal a = Time -> a

newtype SF a b = SF (Signal a -> Signal b)

instance Profunctor SF where
  dimap f g (SF x) = SF (fmap g . x . fmap f)

instance Category SF where
  id = SF id
  SF f . SF g = SF (f . g)

instance Arrow SF where
  arr f = SF (fmap f)
  first (SF f) = SF $ \sbd t ->
    let (b, d) = sbd t
        c      = f (fst . sbd) t
    in (c, d)

--------------------------------------------------------------------------------
-- Sjoerd Visscher's Free Arrows
--
-- http://stackoverflow.com/questions/12001350/useful-operations-on-free-arrows

type x :~> y = forall a b. x a b -> y a b

newtype FreeA p a b = FreeA { runFreeA :: forall q. Switching q => (p :~> q) -> q a b }

evalA :: (Strong q, Switching q, ArrowLoop q) => (p :~> q) -> (FreeA p a b -> q a b)
evalA f a = runFreeA a f

effect :: p a b -> FreeA p a b
effect a = FreeA (\k -> k a)

instance Profunctor (FreeA f) where
  dimap f g (FreeA x) = FreeA (\k -> dimap f g (x k))

instance Strong (FreeA f) where
  first'  (FreeA x) = FreeA (\k -> first'  (x k))
  second' (FreeA x) = FreeA (\k -> second' (x k))

instance Category (FreeA f) where
  id = FreeA (const id)
  FreeA f . FreeA g = FreeA (\k -> f k . g k)

instance Arrow (FreeA f) where
  arr f = FreeA (const $ arr f)
  first  (FreeA f)    = FreeA (\k -> first  $ f k)
  second (FreeA f)    = FreeA (\k -> second $ f k)
  FreeA f *** FreeA g = FreeA (\k -> f k *** g k)
  FreeA f &&& FreeA g = FreeA (\k -> f k &&& g k)

instance ArrowLoop (FreeA f) where
  loop (FreeA x) = FreeA (\k -> loop (x k))

--------------------------------------------------------------------------------
-- Base AFRP

type Event = Maybe

class (Strong p, ArrowLoop p) => Basic p where
  idp    :: p a a
  constp :: b -> p x b

class Basic p => Switching p where
  hold :: p a b -> p (a, Event (p a b)) b
  fuse ::        p  a (b, Event c)
       -> (c -> p  a  b)
       ->       p  a  b
  -- coll ::  (forall p . (a -> [p] -> [(x, p)]))
  --      -> [p x b]
  --      -> p (a, [b]) (Event m)
  --      -> ([p x b] -> m -> p a [b])
  --      -> p a [b]

class Basic p => Timed p where
  nowp   :: p a Time

class Basic p => Smooth p where
  integralp   :: p Double Double
  derivativep :: p Double Double

--------------------------------------------------------------------------------

instance Basic p => Basic (FreeA p) where
  idp      = effect idp
  constp a = effect (constp a)

instance Switching p => Switching (FreeA p) where
  fuse (FreeA x) f = FreeA $ \k -> fuse (x k) (\c -> runFreeA (f c) k)
  hold (FreeA x)   = FreeA $ \k -> lmap (second $ fmap (\p -> runFreeA p k))
                                        (hold (x k))

instance Timed p => Timed (FreeA p) where
  nowp = effect nowp

instance Smooth p => Smooth (FreeA p) where
  integralp   = effect integralp
  derivativep = effect derivativep

--------------------------------------------------------------------------------
-- A nicer interface arrow

infixr 0 ~>
newtype (~>) a b =
  S { unS :: forall p . (Smooth p, Timed p, Switching p) => FreeA p a b }

instance Profunctor (~>) where
  dimap f g (S x) = S (dimap f g x)

instance Strong (~>) where
  first'  (S x) = S (first' x)
  second' (S x) = S (second' x)

instance Category (~>) where
  id = S id
  S f . S g = S (f . g)

instance Arrow (~>) where
  arr f = S (arr f)
  first  (S x) = S (first x)
  second (S x) = S (second x)
  S f *** S g  = S (f *** g)
  S f &&& S g  = S (f &&& g)

instance ArrowLoop (~>) where
  loop (S x) = S (loop x)

instance Basic (~>) where
  idp      = S (effect idp)
  constp b = S (effect $ constp b)

instance Switching (~>) where
  fuse (S x) f = S (fuse x (unS . f))
  hold (S x)   = S (lmap (second (fmap unS)) (hold x))

instance Timed (~>) where
  nowp = S (effect nowp)

instance Smooth (~>) where
  integralp   = S (effect integralp)
  derivativep = S (effect derivativep)

--------------------------------------------------------------------------------

fstp :: (a, b) ~> a
fstp = arr fst

sndp :: (a, b) ~> b
sndp = arr snd

arr2 :: (a -> b -> c) -> (a, b) ~> c
arr2 = arr . uncurry

thetap :: (Double, Double) ~> Double
thetap = arr2 (-) >>> integralp

xp :: (Double, Double) ~> Double
xp = let v = (fstp &&& sndp) >>> arr2 (+)
         t = thetap >>> arr cos
     in (v &&& t) >>> arr2 (*) >>> integralp >>> arr (/2)

yp :: (Double, Double) ~> Double
yp = let v = (fstp &&& sndp) >>> arr2 (+)
         t = thetap >>> arr sin
     in (v &&& t) >>> arr2 (*) >>> integralp >>> arr (/2)

xp' :: (Double, Double) ~> Double
xp' = proc inp -> do
  vr    <- arr snd   -< inp
  vl    <- arr fst   -< inp
  theta <- thetap    -< inp
  i     <- integralp -< ( vr + vl ) * cos theta
  returnA -< i / 2
