{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

module Transducers where

-- There's been a little flurry of activity around analyzing Clojure's
-- transducers using Haskell types. Right now a top contender looks
-- like this:

import           Control.Arrow
import           Control.Category
import           Control.Monad
import           Prelude          hiding (id, (.))

newtype (~>) a b = Transducer (forall r . (b -> r -> r) -> (a -> r -> r))

instance Category (~>) where
  id = Transducer id
  Transducer g . Transducer f = Transducer (f . g)

-- tmap f = cps (return . f)
tmap :: (a -> b) -> (a ~> b)
tmap f = Transducer $ \brr a r -> brr (f a) r

-- tfilt p = cps (mfilter p . return)
tfilt :: (a -> Bool) -> (a ~> a)
tfilt p = Transducer $ \brr a r -> if p a then brr a r else r

tflatMap :: (a -> [b]) -> (a ~> b)
tflatMap f = Transducer $ \brr a r -> foldr (f a) brr r

-- tseq t as = as >>= regular t
tseq :: (a ~> b) -> [a] -> [b]
tseq (Transducer f) = spin where
  spin []     = []
  spin (a:as) = f (:) a (spin as)

-- The intuition here is that a transducer is a transformation between
-- "reducers" which are styled as `(a -> r -> r)` and since `forall r
-- . (a -> r -> r) -> r` is isomorphic to the type of infinite streams
-- it sort of feels like transducers ought to have a type like `a ~> b
-- = [a] -> [b]`.

-- However, Paolo Capriotti [pointed out][0] that there's a *much* easier
-- representation of `a ~> b`. Here's the derivation
--
-- 
--
-- ~~~
-- a ~> b
-- == [ inline ]
-- forall r. (b -> r -> r) -> (a -> r -> r)
-- == [ rearrange ]
-- a -> (forall r . (b -> r -> r) -> r -> r)
-- == [ universal property of lists ]
-- a -> [b]
-- ~~~
-- {: .no-highlight}
--
-- The last step is the most interesting one. Just like how `forall r
-- . (a -> r -> r) -> r` is isomorphic to streams of `a`s via a Church
-- encoding, `forall r . (a -> r -> r) -> r -> r` is isomorphic to
-- finite lists by the same encoding.
--
-- So we can write a function to transform between these two
-- representations

regular :: (a ~> b) -> (a -> [b])
regular (Transducer f) a = f (:) a []

cps :: (a -> [b]) -> (a ~> b)
cps cont = Transducer $ \brr a r -> foldr brr r (cont a)

-- This demonstrates that `Transducer`, as written above, is the
-- Kleisli arrow on the list monad. "Obvious" implementations of
-- `tmap`, `tfilt`, and `tseq` follow naturally

-- ~~~
-- tmap :: (a -> b) -> (a ~> b)
-- tmap f = cps (fmap f . return)
--
-- tfilt :: (a -> Bool) -> (a ~> a)
-- tfilt p = cps (mfilter p . return)
--
-- tseq :: (a ~> b) -> [a] -> [b]
-- tseq t as = as >>= regular t
-- ~~~
-- {: .language-haskell}

-- We can also pretty trivially pick up an `Arrow` instance by
-- borrowing from the structure of the `Kleisli` arrow

instance Arrow (~>) where
  arr   = tmap
  -- first = cps . runKleisli . first . Kleisli . regular
  first (Transducer t) = Transducer $ \cdrr (b, d) -> t (\c -> cdrr (c, d)) b

-- ## So what have we learned?
--
-- Well, *assuming* `Transducer` is a faithful translation of the
-- transducer concept (and Rich Hickey [has suggested][1] that this
-- isn't the case) then tranducers just are a (CPS encoded) way of
-- working with the Kleisli arrows on lists.
--
-- This shows up as well when you try to interpret the `Transducer`
-- type as [a lensy `Traversal`][2] or a [`Monoid` homomorphism][3].
--
-- [1]:http://conscientiousprogrammer.com/blog/2014/08/07/understanding-cloure-transducers-through-types/#comment-1533296409
-- [2]:http://www.reddit.com/r/haskell/comments/2cv6l4/clojures_transducers_are_perverse_lenses/
-- [3]:http://oleksandrmanzyuk.wordpress.com/2014/08/09/transducers-are-monoid-homomorphisms/
--
-- ## What's missing?
--
-- Unfortunately, there's one example use of transducers which totally
-- escapes this representation: Clojure's `take`. It looks a bit like
-- this:
--
-- ~~~
-- (def take
--     ([n]
--      (fn [f1]
--        (let [na (atom n)]
--          (fn
--            ([result input]
--               (let [n @na
--                     nn (swap! na dec)
--                     result (if (pos? n)
--                              (f1 result input)
--                              result)]
--                 (if (not (pos? nn))
--                   (reduced result) ; a terminal value indicating "don't reduce further"
--                   result))))))))
-- ~~~
-- {: .language-clojure}
--
-- For those unfamiliar with Clojure, this code closes over one of
-- Clojure's mutability constructs, an `atom`, to count how many
-- elements have passed through. This lets the transducer maintain a
-- little local state and is required to implement `take`. Sadly,
-- `Transducer` is nowhere near strong enough to represent local state
-- purely.
--
-- My intent isn't to claim that *this* is the reason why `Transducer`
-- is a poor model for Clojure's transducers---indeed, a completely
-- correct model embedded in Haskell would suffer this deficiency just
-- because Haskell cannot have local mutable state like that (unless
-- we embed the whole computation in a monad).
--
-- However, if we beef up what `Transducer`s are then we *can* get a
-- pure `take`. I'll talk about that in a later post.
