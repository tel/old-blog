---
layout: post
title: Mutable Algorithms in Immutable Languages, Part 2
---

Last time we saw a way to implement Union/Find, an algorithm which
depends critically on *observable* mutable memory, within a particular
abstract monad called `Mem`. Monads implementing `Mem` model mutable
memory and we saw that's sufficient to recover Union/Find.

But we didn't actually see any implementations of `Mem`. So maybe
we're hosed. Are there any interesting implementations of `Mem` we can
run Union/Find in?

## In case of emergency break ivory

Okay, I'm being dramatic. There is, of course, at least one such
model. If we're willing to *infect* our program with `IO` then we can
always use `IORef`s to model mutable memory. It's almost too simple,
but we'll do it anyway for completeness and to explain some *nits*
involved in working with the `Mem` class.

Essentially, we'd like to have, more or less, the following
correspondences

* `ref` is `newIORef`
* `deref` is `readIORef`
* `set` is `writeIORef`

Hopefully, this also clears up why I called pointers "refs" back in
part 1.

### Diving in

*The code for this section is available [here](https://github.com/tel/tel.github.io/blob/master/public/code/MutableImmutable/Part2/UnionFind/IO.hs).

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

We'll thus make an entirely new wrapper called, say, `UfIO` for
Union/Find in `IO` and specialize the value to be wrapped in the
`Node_` layer as needed according to the internal `AbstractUnionFind`
interface.

~~~
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

### Phantom parameters

It's worth spending a few seconds focusing on a particular trick used
above. In order to leave the type *stored* in the Union/Find graph
unspecified we've used a *phantom type paramter* on the type of
`UfIO`, the `v`.

The paramter `v` is phantom because it is not used in the
implementation of `UfIO` at all. Instead, the *information* carried by
`v` is only used in the *instantiation* of `Mem`.

This might be a fairly unobvious coding trick, but it can be seen as
the "obviously correct" answer by looking at the semantics of the API
exposed. In particular, `runUfIO` is a function which takes a
computation using the abstract Union/Find and runs it in `IO`. Here's
an example

~~~
z = runUfIO computation where
  computation :: UF r Int => r Bool
  computation = do
    n1 <- node 1
    n2 <- node 2
    link n1 n2
    connected n1 n2
~~~
{: .language-haskell}

We see in `computation` that the result type `r` is constrained to use
`Int` values. If `UfIO` lacked that type parameter then there'd be no
way for our `IO` implementation to *know* that it's working over
`Int`.
