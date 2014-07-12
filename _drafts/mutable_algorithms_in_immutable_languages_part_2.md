---
layout: post
title: Mutable Algorithms in Immutable Languages, Part 2
---

Last time we saw a way to implement Union/Find, an algorithm which
depends critically on *observable* mutable memory, within a particular
abstract monad called `Mem`. Monads implementing `Mem` model mutable
memory (*ed.* say *that* 10 times fast) and we saw that's sufficient
to recover Union/Find.

But we didn't actually see any implementations of `Mem`. So maybe
we're hosed. Are there any interesting implementations of `Mem` we can
run Union/Find in?

## In case of emergency break ivory

Okay, I'm being dramatic. There is, of course, at least one such
model. If we're willing to *infect* our program with `IO` then we can
always use `IORef`s to model mutable memory. It's almost too simple,
but we'll do it anyway for completeness and to explain some nits
involved in working with the `Mem` class.

Essentially, we'd like to have, more or less, the following
correspondences

* `ref` is `newIORef`
* `deref` is `readIORef`
* `set` is `writeIORef`

Hopefully, this also clears up why I called pointers "refs" back in
part 1.

### Diving in

*The code for this section is available [here](https://github.com/tel/tel.github.io/blob/master/public/code/MutableImmutable/Part2/UnionFind/IO.hs).*

So, we could do exactly what was just suggested and implement `IO`
into `Mem` (almost) directly. We'll need a `newtype` wrapper around
`Ref IO` but it's pretty trivial.

~~~
instance Mem IO where
  newtype Ref IO = IORef' { unwrapIORef' :: IORef Int }
  type    Val IO = Int

  ref     = fmap IORef . newIORef
  deref   = readIORef . unwrapIORef'
  set r v = writeIORef (unwrapIORef' r) v
~~~
{: .language-haskell}

Hopefully the major problem with this approach is obvious---`Mem`
restricts us to only store one *type* of thing in memory and `IORef`s
are more general than that. I had to pretty arbitrarily pick `Int`s to
store for this interface.

So this clarifies a point about using `Mem`. It's always going to be
an *internal*, hidden class where instances, probably created using
newtype wrappers, are specific to both a particular use and a
particular algorithm.

(Well, actually, we ought to be able to get around this by using GADTs
and existential types, but I'll leave that for another day.)

We'll thus make an entirely new wrapper called, say, `UfIO` for
Union/Find in `IO` and specialize the value to be wrapped in the
`Node_` layer as needed according to the internal `AbstractUnionFind`
interface.

~~~
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

newtype UfIO v a =
  UfIO { runUfIO :: IO a }
  deriving ( Functor, Applicative, Monad )

instance Mem (UfIO v) where
  newtype Ref (UfIO v) =
    UfIORef { getUfIORef :: IORef (Node_ (UfIO v) v) } deriving ( Eq )
  type Val (UfIO v) = Node_ (UfIO v) v

  ref   a = UfIO (UfIORef <$> newIORef a)
  deref r = UfIO (readIORef $ getUfIORef r)
  set r v = UfIO (writeIORef (getUfIORef r) v)
~~~
{: .language-haskell}

Just a little bit more code and some noise as we wrap and unwrap all
of the `newtype`s (but note that all of that noise is free at
runtime).

### Using `UfIO`

Now that we have something which implements `Mem` with `Val r ~ Node_
r v` we can use it to run our Union/Find algorithm. All we need to do
is construct a computation using the Union/Find interface and then run
it with `runUfIO`.

~~~
ex = runUfIO computation where
  computation :: UF r Int => r Bool
  computation = do
    n1 <- node 1
    n2 <- node 2
    link n1 n2
    connected n1 n2
~~~
{: .language-haskell}

~~~
>>> :t ex
ex :: IO Bool
>>> ex
True
~~~

### Phantom parameters

It's worth spending a few seconds focusing one particular trick used
above. In order to leave the type *stored* in the Union/Find graph
unspecified we've used a *phantom type paramter* on the type of
`UfIO`, the `v`.

The paramter `v` is called "phantom" because it is not used in the
implementation of `UfIO` at all. Instead, the *information* carried by
`v` is only used in the *instantiation* of `Mem`.

This phantom parameter is almost entirely what allows us to avoid the
problem we encountered above with the `Mem` instance of `IO`. The only
way for the associated type `Val (UfIO v)` to change is if we use
information about some extra parameter like `v`.

Phantom parameters will return in this series. Generally, their
ability to *constrain* values and subsequently expose more information
about them is a crticial trick.

## Recovering purity

Whew! With all that `IO` out of the way we can get back to solving
real fake problems like how to *emulate* mutable memory.

We've seen that what we need to do is establish the ability to store
objects of type `Val r` and produce references, `Ref r`, to them which
can be read from or written to. Without beating around the bush too
much, this behavior is very similar to that of storing and accessing
values from a finite `Map`. Better, if we just let our `Ref`s be
integer identifiers we can use the efficient [`Data.IntMap.IntMap`](http://hackage.haskell.org/package/containers-0.5.5.1/docs/Data-IntMap.html)
from [the `containers` package](http://hackage.haskell.org/package/containers)

(There's a question as to whether or not we should use the *lazy* form
of IntMap, but I'm going to ignore it.)

If we wrap one of those maps up into
[a `State` monad](http://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-State.html)
then monadic operations which edit the finite map by using integer
references is basically what `Mem` asks for. Better yet, we can just
"snapshot" our memory at any point (using
[`get`](http://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-State-Strict.html#v:get))
to escape back into purity.

Sounds perfect. Let's build it!

### Implementing purely mutable memory

As described, we're going to implement a `Monad` which is not a whole
lot more than a `State` monad carrying around our `IntMap`
representing memory.

~~~
newtype UfIntMap v a =
  UfIntMap { unUfIntMap :: State (Uf v) a }
  deriving ( Functor, Applicative, Monad )
~~~
{: .language-haskell}

and as long as we can generate some initial state (`uf0`) then we can
"run" a `UfIntMap` computation purely, shedding its monadic layer.

~~~
runUfIntMap :: UfIntMap v a -> a
runUfIntMap = flip evalState uf0 . unUfIntMap where
~~~
{: .language-haskell}

That state type `Uf v` isn't exactly what I may have led you to
believe it would be. Instead of only holding an `IntMap` we must also
keep a "source" of IDs to ensure we don't collide.

~~~
data Uf v =
  Uf { count :: Int
     , mem   :: IntMap (Node_ (UfIntMap v) v)
     }

uf0 :: Uf v
uf0 = Uf { count = 0, mem = IM.empty }
~~~
{: .language-haskell}

But that's really just a small technical detail. All that's left now
is to implement the `Mem` instance. We use the same phantom variable
trick from `UfIO v`, but now references are just wrappers over `Int`s
in our map.

~~~
instance Mem (UfIntMap v) where
  newtype Ref (UfIntMap v) =
    UfIntMapRef { getId :: Int } deriving ( Eq )
  type Val (UfIntMap v) = Node_ (UfIntMap v) v
~~~
{: .language-haskell}
