---
layout: post
title: Stupid right/left folding tricks
comments_enabled: true
excerpt: There's a folk-wisdom trick for converting a right fold on lists into a left fold. This may not appear to be terrifically useful at first but if you look a bit beneath the surface then it reveals a practical tool in any functional programmer's toolbelt.
---

Here's a stupid FP trick that "everybody" knows. I'm going to define
`foldl` in terms of `foldr`. In other words, given values `snoc :: r
-> a -> r` and `leftNil :: r` I'm going to produce the values `cons ::
a -> r -> r` and `rightNil :: r` such that `foldr cons rightNil =
foldl snoc leftNil`.

~~~
foldl :: forall a r . (r -> a -> r) -> r -> ([a] -> r)
foldl snoc leftNil as = (foldr cons rightNil as) leftNil where
  cons :: a -> (r -> r) -> r -> r
  cons a f r = _ snoc (f r) a
  rightNil :: r -> r
  rightNil = id
~~~
{: .haskell}

Take note of the use of `ScopedTypeVariables` so that my inner types
correspond correctly with the outer one. I've already done most of the
tricky bits, but I left a type hole for `cons` as it's worth examining
in more depth. In particular, there are two type checking
implementations:

~~~
cons a f r = snoc (f r) a   -- this one is wrong
cons a f r = f (snoc r a)
~~~
{: .haskell}

If you choose the first, wrong one then you end up defining "`foldr`
with `foldr`", but if you choose the second then you get the desired
`foldl` semantics as desired.

## What's going on?

What I wrote above is the trick, but what does it mean?

The short answer is that we simulate a fold from the left by folding a
*function* from the right. This function begins as `id` but as this
result bubbles up from the right side of your list it accumulates
applications of `snoc`. If these snocs are composed on the "pre" or
"right" side of this accumulating function then they'll be applied in
reverse order---effectively, this simulates your left fold. If they're
composed on the "post" or "left" side then they end up in normal order
and you get a right fold.

To emphasize the heart of the trick: we use `foldr` to generate a
summary of the list which is a *function* and depending on how that
function is built during the fold indicates whether information flows
from the left or from the right.

## Generalizing the trick

Where this ends up being useful is when you want to use a right fold
but need to send information "from the left". For instance,
enumerating a list using `foldr` in a single pass. We'd like `numerate
:: [a] -> [(Int, a)]` to be written as a right fold such that
`numerate "abc" == [('a', 1), ('b', 2), ('c', 3)]`. If we do
it naively we'll get

~~~
numerate' :: [a] -> [(Int, a)]
numerate' = snd . foldr (\(n, acc) a -> (n+1, (n, a) : acc)) (1, [])
~~~
{: .haskell}

This won't work as it has `numerate' "abc" ==
[('a', 3), ('b', 2), ('c', 1)]` which we might have been clued in to
since the initial seed of the fold pairs `1` with `[]`.

Instead, we want to pass the current count "from the left" using the
function trick from before. In other words, we'd like to write a
`foldr` which results in a function that takes an `Int` and produces
the enumerated list

~~~
numerate :: [a] -> [(Int, a)]
numerate as = go 1 where
  go :: [a] -> (Int -> [(Int, a)])
  go = foldr cons nil as

  cons :: a -> (Int -> [(Int, a)]) -> (Int -> [(Int, a)])
  cons a future s = (s, a) : future (s+1)

  nil :: Int -> [(Int, a)]
  nil = const []
~~~
{: .haskell}

And now, `numerate "abc" == [(1,'a'),(2,'b'),(3,'c')]` like we
wanted. This "fold from the right, pass state from the left" idiom
shows up from time to time and is a useful trick to have in your
toolbox.

But that's a pretty unsatisfactory way to think about it. This is
functional programming! We solve problems like this through sufficient
abstraction.

## Packing up the trick

So, let's blow this up a little bit. Consider the definition of
numerate above

~~~
numerate :: [a] -> [(Int, a)]
numerate as = foldr
              (\a future s -> (s, a) : future (s+1))
              (const [])
              as
              1
~~~
{: .haskell}

and let's identify the pieces which are specific to numeration like
the `(:)` and `[]` specific to the task of building up the result list
and the `1` and `(+)` which is specific to the type of "state" being
passed forward. If we abstract over those we'll get a new way of
looking at folds.

~~~
collapse :: (a -> l -> l) -> (a -> (l -> r -> r)) -> ([a] -> (l -> r -> r))
collapse upd step as l0 r0 =
  foldr
  (\a future l -> step a l (future (upd a l)))
  (const r0)
  as
  l0

numerateCollapse :: [a] -> [(Int, a)]
numerateCollapse as = collapse (const succ) (\a l r -> (l, a) : r) as 1 []
~~~
{: .haskell}

The combinator `collapse` isn't exactly clear, but its type tells an
interesting story. The key is to examine the "result" type `[a] -> (l
-> r -> r)` and see it as converting a list to a function that takes
"left" and "right" state to a final right state. I'm calling it a
"collapse" since it seems to eke out the result by standing in the
middle of waves of state coming from the left and right, the past and
the future.

We generate this final result by taking the "stepwise collapse" `a ->
(l -> r -> r)` which does the same thing using only a single element
of the list and the "most proximal" left and right states. For
example, if we call `numerateCollapse "abcde"` then one "stepwise
collapse" is passed the values `step 'c' 3 [(4, 'd'), (5, 'e')]` where
the `3` is the "left state" of the increasing count and the
`[(4, 'd'), (5, 'e')]` is the "right state" of the growing completed
computation.

We're also given a chance to update the leftward state using `a -> l
-> l`. This is clearly vital for numeration, but it sort of feels like
we ought to be able to unify the left update and the stepwise
collapse.

~~~
collapse2 :: ( a  -> (l, r) -> (l, r)) ->
             ([a] -> (l, r) -> (l, r))
collapse2 step as (s0, z0) =
  foldr
  (\a future l0 ->
       let (l1, r1) = step a (l0, r0)
           (l2, r0) = future l1
       in (l2, r1))
  (\s -> (s, z0))
  as
  s0
~~~
{: .haskell}

Without making any claim that this is a good idea, here we go. The
`step` now is responsible for *permuting* the states from the left and
right across the current point. Since this is done all at once we have
the opportunity to mix signals from the past and the future and
produce infinite loops. But we can also just write a normal numeration
function again

~~~
numerateCollapse2 :: [a] -> (Int, [(Int, a)])
numerateCollapse2 as = collapse2 (\a (l, r) -> (l+1, (l, a) : r)) as (1, [])
~~~
{: .haskell}

## Practical collapses

Honestly, the `collapse` combinator is a terrifically complicated
thing that should almost certainly never live in real code. But, on
the other hand, the "left-flowing" state in a right fold is a really
useful trick that shows up from time to time.

My primary motivation for `collapse` was to explore the idea a little
further and see if we couldn't reflect what was going on up into the
types. Perhaps it helps or perhaps it serves only to further
mystify. If you're really interested in functionality like this then
you should take a look at the
[Tardis](http://hackage.haskell.org/package/tardis) monad as well.
