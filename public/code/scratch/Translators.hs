{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs, RankNTypes, StandaloneDeriving, ScopedTypeVariables #-}

module Translators where

import Control.Applicative
import Control.Category
import Control.Monad
import Data.List
import Prelude hiding ((.), id)

--------------------------------------------------------------------------------

newtype I a = I { runI :: a } deriving Functor
instance Monad I where
  return = I
  I a >>= f = f a
instance Applicative I where
  pure = return
  (<*>) = ap
  
--------------------------------------------------------------------------------

class Functor w => Comonad w where
  extract :: w a -> a
  extend :: (w a -> b) -> (w a -> w b)
  extend f = fmap f . duplicate
  duplicate :: w a -> w (w a)
  duplicate = extend id

class Profunctor p where
  dimap :: (i' -> i) -> (o -> o') -> (p i o -> p i' o')
  dimap f g = lmap f . rmap g
  lmap :: (i' -> i) -> (p i o -> p i' o)
  lmap f = dimap f id
  rmap :: (o -> o') -> (p i o -> p i o')
  rmap f = dimap id f

--------------------------------------------------------------------------------

data Moore i o where
  Moore :: (x -> i -> x) -> x -> (x -> o) -> Moore i o

instance Functor (Moore i) where
  fmap = rmap

instance Profunctor Moore where
  dimap f g (Moore xix x xo) =
    let xix' x i = xix x (f i)
        x'       = x
        xo'  x   = g (xo x)
    in  Moore xix' x' xo'

newtype T m i o = T { unT :: forall r . Moore o r -> m (Moore i r) }

instance Functor (T m i) where
  fmap f (T m) = T $ \(Moore xox x xr) ->
    let xox' x o = xox x (f o)
    in m (Moore xox' x xr)

instance Functor m => Profunctor (T m) where
  dimap f g (T m) = T $ \(Moore xox x xr) ->
    let xox' x o = xox x (g o)
        back (Moore yiy y yr) =
          let yiy' y i = yiy y (f i)
          in  Moore yiy' y yr
    in back <$> m (Moore xox' x xr)
    
instance Monad m => Category (T m) where
  id = T return
  T f . T g = T (g <=< f)

tseq :: Functor m => T m i o -> [i] -> m [o]
tseq (T m) fi = build <$> m mach0 where
  mach0 = Moore (\x a -> x . (a :)) id ($ [])
  build (Moore xix x xr) = xr (foldl' xix x fi)

tseq' :: T I i o -> [i] -> [o]
tseq' t = runI . tseq t

tmap :: Applicative m => (i -> o) -> T m i o
tmap f = T (pure . lmap f)

tfilter :: Applicative f => (a -> Bool) -> T f a a
tfilter p = T $ \(Moore xax x xr) ->
  pure (Moore (\x a -> if p a then xax x a else x) x xr)

tremove :: Applicative f => (a -> Bool) -> T f a a
tremove p = tfilter (not . p)

data Two a b = Two !a !b deriving ( Eq, Ord, Show, Functor )

ttake :: Applicative f => Int -> T f a a
ttake n0 = T $ \(Moore xax x xr) ->
  let xax' (Two mn x) a = case mn of
        Nothing -> Two Nothing x
        Just n | n > 0     -> Two (Just $ n-1) (xax x a)
               | otherwise -> Two Nothing x
      x'            = Two (if n0 >= 0 then Just n0 else Nothing) x
      xr' (Two _ x) = xr x
  in pure (Moore xax' x' xr')

ttakeWhile :: Applicative f => (a -> Bool) -> T f a a
ttakeWhile p = T $ \(Moore xax x xr) ->
  let xax' (Two ok x) a =
        if ok
        then if p a
             then Two True  (xax x a)
             else Two False x
        else Two False x
      x'            = Two True x
      xr' (Two _ x) = xr x
  in pure (Moore xax' x' xr')

tdrop :: Applicative f => Int -> T f a a
tdrop n0 = T $ \(Moore xax x xr) ->
  let xax' (Two mn x) a = case mn of
        Nothing -> Two Nothing (xax x a)
        Just n | n > 0     -> Two (Just $ n-1) x
               | otherwise -> Two Nothing (xax x a)
      x'            = Two (if n0 >= 0 then Just n0 else Nothing) x
      xr' (Two _ x) = xr x
  in pure (Moore xax' x' xr')

tdropWhile :: Applicative f => (a -> Bool) -> T f a a
tdropWhile p = T $ \(Moore xax x xr) ->
  let xax' (Two ok x) a =
        if ok
        then if p a
             then Two True x
             else Two True (xax x a)
        else Two False x
      x'            = Two True x
      xr' (Two _ x) = xr x
  in pure (Moore xax' x' xr')

ttakeNth :: Applicative f => Int -> T f a a
ttakeNth m = T $ \(Moore xax x xr) ->
  let xax' (Two i x) a
        | i == m    = Two 1     (xax x a)
        | otherwise = Two (i+1) x
      x'            = Two 1 x
      xr' (Two _ x) = xr x
  in pure (Moore xax' x' xr')

tflatMap :: Applicative f => (a -> [b]) -> T f a b
tflatMap f = T $ \(Moore xbx x xr) ->
  pure (Moore (\x a -> foldl' xbx x (f a)) x xr)

-- replace (Map k v -> f k -> b v)
-- partition-by ((a -> Bool) -> Transducer m i [i]) split on trigger
-- partition-all (Int -> Transducer m i [i])
-- dedup
-- random-sample (Double -> Transducer (Rand m) i i)
-- iteration
--     "Returns an iterable/seqable/reducible sequence of applications of
--     the transducer to the items in coll. Note that these applications
--     will be performed every time iterator/seq/reduce is called."

--------------------------------------------------------------------------------

-- Like `Moore` but cancellable. Currently these are recursive which
-- will hurt performance quite a lot.

-- This might be a good non-recursive formulation
data M' i o where
  M' :: (i -> x -> Either o x) -> Either o x -> (x -> o) -> M' i o

data M i o where
  Open   :: o -> (i -> M i o) -> M i o
  Closed :: o                 -> M i o

instance Functor (M i) where
  fmap = rmap

instance Profunctor M where
  dimap f g = z where
    z m = case m of
      Open   o go -> Open   (g o) (z . go . f)
      Closed o    -> Closed (g o)

instance Comonad (M i) where
  extract m = case m of
    Open   o _ -> o
    Closed o   -> o
  extend f m = case m of
    Closed o    -> Closed (f m)
    Open   o go -> Open   (f m) (extend f . go)

newtype Q n i o =
  Q { runQ :: forall r . n o r -> n i r }

instance (Profunctor n) => Functor (Q n i) where
  fmap = rmap

instance (Profunctor n) => Profunctor (Q n) where
  dimap f g (Q t) = Q (lmap f . t . lmap g)

instance Category (Q n) where
  id = Q id
  Q f . Q g = Q (g . f)

mach0 :: M o ([o] -> [o])
mach0 = z id where z now = Open now (\i -> z (now . (i:)))

feed :: [i] -> M i o -> M i o
feed is m = case is of
  []      -> m
  i : is' -> case m of
    Open o go -> feed is' $! go i
    _         -> m

sseq :: Q M i o -> [i] -> [o]
sseq (Q q) is = extract (feed is (q mach0)) []

sflatMap :: forall i o . (i -> [o]) -> Q M i o
sflatMap f = Q z where
  z :: forall r . M o r -> M i r
  z m = case m of
    Closed r    -> Closed r
    Open   r go -> Open   r (z . flip feed m . f)
