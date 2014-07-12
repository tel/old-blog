---
layout: post
title: Mutable Algorithms in Immutable Languages, Part 1
---

[*All code for this post is available in the blog repository on Github.*](https://github.com/tel/tel.github.io/tree/master/public/code/MutableImmutable/Part1)

One of the big challenges you might face when learning a language like
Haskell which favors immutability or purity is that suddenly all of
the algorithms you once kept at your side have... well, by-and-large
they've vanished.

Many "imperative" algorithms rely upon mutable memory to work and it
can be a challenge to remove that dependency. Usually in the process
you'll learn quite a bit about
[purely functional data structures](http://www.amazon.com/Purely-Functional-Structures-Chris-Okasaki/dp/0521663504)
and the flow of data in the algorithm itself. It can be really
enlightening.

But for those times when you just want an algorithm without all the
enlightenment this can put a damper on your day.

Worst of all, some algorithms have no known immutable cousin. One of
my favorite algorithms, Union/Find, which lets you compute connected
components on a graph, is such an algorithm. In order to achieve its
great efficiency Union/Find relies critically upon observable
mutability.

## Modeling mutability

[Of course there's `IO`.](http://hackage.haskell.org/package/union-find-0.2/docs/Data-UnionFind-IO.html)
but it sucks to drop `IO` into the middle of your algorithm. If we
want to avoid it we have to do something strange: we'll model *memory
itself* purely. Then Union/Find will live somewhere nice like a `State
Mem` monad.

Ah, *monads*, I can smell the purity already. It's invigorating.

In fact, what we'll build here can be used to model pointer arithmetic
nicely and thus whatever mutable algorithm you want. At its most pure
we'll see that this costs $$O(\log(n))$$ for $$n$$ pointers, but more
efficient models exist as well with small caveats.

### Abstract mutability

But first, let's talk about what we mean when we say "mutability" or
"memory". First, let's not pretend at all that this will be a pure
interface---we'll need a monad.

~~~
class Monad r => Mem r where ...
~~~
{: .language-haskell}

The basic operations of mutability involve creating new pointers from
values, dereferencing pointers, and storing new values at a point.
We'll call pointers "references" (with reasons to come) and write an
abstract API like this

~~~
{-# LANGUAGE TypeFamilies #-}

class (Monad r, Eq (Ref r)) => Mem r where
  data family Ref r :: *
  type family Val r :: *

  ref   :: Val r -> r (Ref r)
  deref :: Ref r -> r (Val r)
  set   :: Ref r -> Val r -> r ()
~~~
{: .language-haskell}

In other words, we are going to produce mutability effects in *some*
`Monad` named by the variable `r`. It'll have two *associated types*,
`Ref r` and `Val r` which are the *references* and *values* under this
effect. Notably, each mutability monad will be allowed to store values
at only one type, `Val r`. Also notably, we require that references
have equality even when values do not---this is "pointer equality" or
"entity" identity!

Using this abstract implementation we can begin to build more complex
`Mem` actions. For instance, a useful higher-order action is to
combine `deref` and `set` into `alter`.

~~~
alter :: Mem r => (Val r -> Val r) -> Ref r -> r ()
alter f r = do
  v <- deref r
  set r (f v)
~~~
{: .language-haskell}

Note the signature: `alter` works for *any* `Mem` monad and we can see
that directly in the types.

## Union/Find

Is `Mem` a sufficient model? Turns out, yes! We can write a Union/Find
algorithm that depends only upon the effect basis defined by `Mem`.

Union/Find, as stated briefly above, allows us to find connected
components in graphs. We do this by asserting nodes and links and
retrieving references for each node we assert.

~~~
{-# LANGUAGE ConstraintKinds #-}

-- | This constraint indicates that we can Union/Find values
--   `a` in the monad `r`
type UF r a

-- | This is a mutable reference to a node in a Union/Find graph
newtype Node r

node :: UF r a => a                -> r (Node r)
link :: UF r a => Node r -> Node r -> r ()
~~~
{: .language-haskell}

Finally, we can determine whether two references are transitively
linked using the `connected` function.

~~~
connected :: UF r a => Node r -> Node r -> r Bool
~~~
{: .language-haskell}

### Implementing abstract Union/Find

Finally we can write the implementation of Union/Find by creating
implementations of these interface types. First we note that `Node` is
nothing more than a thin wrapper over `Ref` which obscures equality.

~~~
newtype Node r = Node (Ref r)
~~~
{: .language-haskell}

We write `connected` in terms of another internal function called
`find` (this is the "find" in the name of the algorithm; `link` is the
"union"). `find` takes any `Node` and returns another `Node` which is
the "representative" node for some connected component in the graph.
Union/Find works by maintaining the invariant that two `Node`s are in
the same connected component iff their representative nodes are the
same.

~~~
find :: UF r a => Node r -> r (Node r)

connected :: UF r a => Node r -> Node r -> r Bool
connected n1 n2 = do
  Node p1 <- find n1
  Node p2 <- find n2
  return (p1 == p2)
~~~
{: .language-haskell}

To go further we have to consider two optimizations of Union/Find
which give it the performance properties we seek. Firstly, `find`
*caches* the representative of a `Node` by using path compression
(we'll see the path shortly). Secondly, we'll find that `link` is not
commutative and we improve efficiency by picking the right "direction"
to `link`. Fortunately, there's an easy heuristic called `rank` which
we can track.

To not delay the point any longer, a Union/Find `UF r a` actually
stores references to a `Node_` struct which builds a tree of values

~~~
type UF r a = (Mem r, Val r ~ Node_ r a)

data Node_ r a =
  Node_ { parent :: Maybe (Ref r)
        , rank   :: Int
        , value  :: a
        }
~~~
{: .language-haskell}

Here, `parent` points at `Just` the parent `Node_` or `Nothing` if
we're considering a root `Node_`; `rank` stores the "rank" heuristic
of a `Node_`; and `value` is just the stored value.

Then, having gotten `Node_` we can figure out what `UF` must mean. It
constrains `r` and `a` such that (1) `r` instantiates `Mem` and (2)
`Val r` is of the form `Node_ r a`. (It's perhaps unfortunate to use
mechanics like associated types and `(~)` type equivalence
constraints, but syntax aside they provide exactly the right meaning
to describe `Mem`.
[Here's a lightning-fast intro to the syntax](http://nattermorphisms.blogspot.com/2008/10/2-minute-intro-to-associated-types-type.html)
and
[here's a more leisurely example of using associated types](https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/type-families-and-pokemon).)

At this point we've almost got a complete picture of the algorithm! We
introduce new (rank-0) `Node`s by making new roots in our Union/Find
forest being careful to hide the implementation using the `Node`
wrapper.

~~~
node :: UF r a => a -> r (Node r)
node a = do
  r <- ref (Node_ { parent = Nothing, rank = 0, value = a })
  return (Node r)
~~~
{: .language-haskell}

We implement `find` by jumping upward following `parent` links until
we find a root node which is our representative node. Since we've now
traversed this path once we'd like to never do it again, so we cache
the result.

~~~
find :: UF r a => Node r -> r (Node r)
find (Node r) = do
  Node p <- findRec (Node r)

  -- PATH COMPRESSION
  -- If we began at the top we don't want to rewrite the parent
  -- but if we're didn't then we cache the root
  unless (r == p) $ alter (\n -> n { parent = Just p }) r

  return (Node p)

  where
    -- | Recursively jump up `parent` links until we're
    --   at a root node
    findRec :: UF r a => Node r -> r (Node r)
    findRec (Node r) = do
      n <- deref r
      case parent n of
        Nothing -> return (Node r)
        Just p  -> find (Node p)
~~~
{: .language-haskell}

Finally, we must implement `link`. To link two `Node`s we `find` each
`Node` representative. If the reps are already identical we're done,
otherwise we have one representative "adopt" the other. We perform
this adoption such that the lesser ranked `Node` becomes the child.
When the two nodes have equal rank then we pick one arbitrarily to
become parent *and* increase the parent's rank.

A little analysis will show that `rank` would be nothing more than
tree height if we didn't also perform path compression which ought to
be a big hint as to why it's the right heuristic.

~~~
link :: UF r a => Node r -> Node r -> r ()
link n1 n2 = do
  Node p1 <- find n1
  Node p2 <- find n2
  unless (p1 == p2) (adopt p1 p2)
  where
    adopt x y = do
      nx <- deref x
      ny <- deref y
      case compare (rank nx) (rank ny) of
        EQ -> do set x (nx { rank   = rank nx + 1 })
                 set y (ny { parent = Just x      })
        LT -> set x (nx { parent = Just y })
        GT -> set y (ny { parent = Just x })
~~~
{: .language-haskell}

And it's a wrap! Here's Union/Find in Haskell.

Sort of. Actually we haven't really done anything. This whole exercise
has been predicated upon an unproven assertion.

*We still have to find a model of mutable memory which can implement
the `Mem` interface.*

But that will have to come a little later. It's slightly trickier than
it looks. For anyone eager to jump ahead, try implementing it using
`IO` and `State S` where `S` includes an `IntMap`. The simple version
using an `IntMap` will have a subtle API bug which will allow bad
state to seep in.

Until next time!
