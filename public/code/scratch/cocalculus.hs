{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cocalculus where

import           Data.Functor.Foldable
import           Prelude               hiding (head, tail)

-- Based on Pavlovic and Escardo's Calculus in Coinductive Form
-- <http://www2.tcs.ifi.lmu.de/~miranda/public/cocalculus.pdf>

-- Generalized coalgebras

jam :: f () -> Nu f
jam f_ = Nu (const f_) ()

-- Coalgebraic final streams

data Stream_ a x = Stream_ { here :: !a, there :: x } deriving Functor
type Stream a = Nu (Stream_ a)

head :: Stream a -> a
head = here . project

tail :: Stream a -> Stream a
tail = there . project

(-:) :: a -> Stream a -> Stream a
a -: s = embed (Stream_ a s)

rep :: a -> Stream a
rep a = jam (Stream_ a ())

view :: Int -> Stream a -> [a]
view 0 _ = []
view n s = let Stream_ h t = project s in h : view (n-1) t

-- Analytic functions

-- newtype Fun a = Fun (Stream a) deriving (Foldable, Unfoldable)
