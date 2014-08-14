{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeOperators             #-}

module Transducers where

import           Control.Applicative
import           Control.Category
import           Control.Comonad
import qualified Data.Foldable       as F
import           Data.List
import           Data.Profunctor
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Prelude             hiding (id, (.))

-- [Last time][last-time] I encoded Clojure transducers as functions
-- like `forall r . (b -> r -> r) -> (a -> r -> r)` and then explored
-- an isomorphism between that type and the type of Kleisli arrows on
-- lists: `a -> [b]`.
--
-- This isomorphism suggested some reasoning for why transducers have
-- some of the properties that they do, but fell short of implementing
-- all of the functionality that Clojure transducers have. One example
-- is `ttake :: Int -> (a ~> a)` which does truncation. In Clojure
-- `ttake` can be implemented by using some closed-over local state
-- via an `atom`, but Haskell affords no such impurity.
--
-- In this post, I'll explore an empowered encoding of transducers
-- which *does* allow for a pure implementation of `ttake`. But to do
-- that, we'll have to begin again with reducers.
--
-- [last-time]:http://tel.github.io/2014/08/10/typing-transducers/


-- ## Reducers are left folds
--
-- Reducers in Clojure join reduction functions like `forall r . (a ->
-- r -> r)` to "reducible" containers. The brunt of the matter is that
-- you can transform reducers by transfoming the reduction function
-- and delay actually touching the adjoined container until a later
-- time of your choosing.
--
-- Thus you get automatic fusion. But it's a little weird since most
-- of the action is occurring within the algebra of reduction
-- functions and the adjoined reducible container just sits until a
-- final call to `r/reduce` or `r/fold`[^actions]. Transducers seem to
-- be an obvious step in retrospect here---focus on the algebra of the
-- reduction functions, not the whole reducer!

-- [^actions]: This is a bit like the difference between examining a
-- group and examining its *actions*. Looking at actions can be great
-- for understanding the functionality of the group, but when we do
-- that we're really studying the group.

-- So let's do that.

-- ### Reduction function junction
--
-- The type of a left fold reduction over elements `a` in Haskell
-- might be `forall r . r -> a -> r`, but there's been a lot of work
-- in this space already which produced the concept of a left fold as
-- *automata*. In particular, we'll be interested in the [left-fold
-- Moore Machine][moore-machine] representation:
--
-- [moore-machine]:http://hackage.haskell.org/package/folds-0.6.2/docs/Data-Fold.html#t:L
--

data Red a b where
  Red :: (x -> b) -> (x -> a -> x) -> x -> Red a b

instance Profunctor Red where
  dimap f g (Red xc xbx x) = Red (g . xc) (\x -> xbx x . f) x

instance Functor (Red i) where
  fmap = dimap id

instance Comonad (Red i) where
  extract (Red xb _ x) = xb x
  extend e (Red xb xax x) = Red (e . Red xb xax) xax x

data P a b = P !a !b

instance Applicative (Red i) where
  pure o = Red (const o) const ()
  Red xf xixf x0 <*> Red ya yiya y0 =
    Red (\(P x y)   -> xf x $ ya y)
        (\(P x y) i -> P (xixf x i) (yiya y i))
        (P x0 y0)

fold :: Red i o -> [i] -> o
fold (Red xo xix x) = xo . foldl' xix x

newtype (~>) a b =
  Transducer (forall o . Red b o -> Red a o)

instance Category (~>) where
  id = Transducer id
  Transducer f . Transducer g = Transducer (g . f)

-- | Uses the Cayley/difference list representation to build the list
-- via appends
tseq :: (a ~> b) -> ([a] -> [b])
tseq (Transducer f) = fold (f (Red ($ []) (\x a -> x . (a:)) id))

tset :: Ord b => (a ~> b) -> ([a] -> Set b)
tset (Transducer f) = fold (f (Red id (flip Set.insert) Set.empty))

tmap :: (a -> b) -> (a ~> b)
tmap f = Transducer (dimap f id)

tfilt :: (a -> Bool) -> (a ~> a)
tfilt p = Transducer $ \(Red xr xax x) ->
  Red xr (\x a -> if p a then xax x a else x) x

tflatMap :: (a -> [b]) -> (a ~> b)
tflatMap f = Transducer $ \(Red xr xbx x) ->
  Red xr (\x a -> foldl' xbx x (f a)) x

ttake :: Int -> (a ~> a)
ttake n = Transducer $ \(Red xr xax x) ->
  Red (\(P x _) -> xr x)
      (\(P x n) a -> if n > 0 then P (xax x a) (n-1) else P x n)
      (P x n)

-- In other words, we think of a reducing function as being a
-- combination of three pieces: (1) an internal state value of type
-- `x`, (2) an "update" function of type `x -> a -> x` which takes
-- inbound `a` values and updates the internal state, and (3) an
-- output mapping of type `x -> b` which transforms the internal state
-- into an output value.

-- This is notably a good bit more complex than reducers are usually
-- considered to be, but it provides a very important notion! In p

data MooreN i o where MooreN :: (r      -> o) -> (r -> i -> r) -> r -> MooreN i o
data MealyN i o where MealyN :: (r -> i -> o) -> (r -> i -> r) -> r -> MealyN i o

newtype MooreM i o = MooreM (forall r. ((i -> r)       -> o  -> r) -> r)
newtype MealyM i o = MealyM (forall r. ((i -> r) -> (i -> o) -> r) -> r)

data MooreF i o = MooreF o (i -> MooreF i o)
data MealyF i o = MealyF (i -> (o, MealyF i o))

------------------------------------------------------------------------

rmap :: (a -> b) -> Reducer a r -> Reducer b r
rmap f (R s xf) = R s (xf . mapping f) where
  mapping f f1 result input = f1 result (f input)

rfilter :: (a -> Bool) -> Reducer a r -> Reducer a r
rfilter p (R s xf) = R s (xf . filtering p) where
  filtering pred f1 result input = if pred input then f1 result input else result

rflatMap :: (a -> [b]) -> Reducer a r -> Reducer b r
rflatMap t (R s xf) = R s (xf . mapcatting t) where
  mapcatting f f1 result input = foldl' f1 result (f input)

data Reducer a r where
  R :: F.Foldable s => s x -> ((r -> a -> r) -> (r -> x -> r)) -> Reducer a r

reducer :: F.Foldable s => s a -> Reducer a r
reducer s = R s id

reduce :: Reducer a r -> (r -> a -> r) -> r -> r
reduce (R s xf) f x0 = F.foldl' (xf f) x0 s

reduceList :: Reducer a ([a] -> [a]) -> [a]
reduceList (R s xf) = F.foldl' (xf (\f a -> f . (a:))) id s []

------------------------------------------------------------------------

data (<) a b = Elaborates !(a -> b) !(b -> a)

inject :: a < b -> (a -> b)
inject (Elaborates f _) = f

retract :: a < b -> (b -> a)
retract (Elaborates _ g) = g

instance Category (<) where
  id = Elaborates id id
  Elaborates f g . Elaborates h j = Elaborates (f . h) (j . g)

type Redu a r = r -> a -> r

data T a b where
  T :: (forall q . q -> Redu b q -> (q < s, Redu a s)) -> T a b

t :: T a b -> (a ~> b)
t (T z) = Transducer $ \(Red so sis s) ->
  let (elab, red) = z s sis
  in Red (so . retract elab) red (inject elab s)
       
       
