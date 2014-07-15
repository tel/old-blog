---
layout: post
title: Mutable Algorithms in Immutable Languages, Part 3
---

*Or: a love song to rank-2 types and phantom variables.*

*See also [part 1][part 1] and [part 2][part 2]. All of the code in
 this post [is available in the blog repository on Github][repo].*

In [part 1][part 1] we demonstrated that we can write algorithms which
require mutable references by embedding them in an abstract monad,
which I called `Mem`, that is adequate to describe a sequential
process of creating, reading, and writing from mutable references. In
[part 2][part 2] we instantiated this monad twice, first impurely and
then later purely, and showed that these implementations sufficiently
modeled the interface allowing our mutable algorithms to be executed.

But we also saw a flaw in the pure approach. The API allows us to
build computations which may fail violate our expectations for how
mutable references work---it's easy to construct "valid looking" uses
which throw errors or behave unexpectedly.

I called the exposed flaw a "border crossing bug". Subtly, one might
have noticed that this bug could not arise in the impure, `IO`-based
implementation. If you didn't notice that, then I encourage you to try
replicating the buggy code that used `UfIntMap` using `UfIO`.

What I'd like to suggest is that this bug demonstrates the tension
between pure and impure computations.

In this part I plan not only to resolve the bug by leaning on
Haskell's type system, but also to explore what it demonstrates, to
determine what exactly mutable algorithms demand from a language, and,
finally, to show how my fancily-typed solution and its cousin,
Haskell's `ST` monad, provide an adequate foundation for mutation.

* http://galois.squarespace.com/storage/files/downloads/publications-jl/lazy-functional-state-threads.pdf

## The unstable state of things

So let's really dive into this bug. Replicated from before:

~~~
bug =
  let
    n1 = runUfIntMap $ do
      node ()
      node ()
      node ()
    conn = runUfIntMap $ do
      n2 <- node ()
      connected n1 n2
  in
   conn
~~~
{: .language-haskell}

You might have argued that this is an obviously improper use of the
API afforded by `Mem` and `runUfIntMap`. In particular, each time
`runUfIntMap` is called the computation, its argument, is "run" in a
completely fresh environment. References from other environments are
not allowed as they do not have any meaning in a fresh environment.

Another way to examine the flaw here is to notice that there are two
complementary violations occurring. First, in the block named by `n1`
we allow a reference to "escape" its region of meaning by returning
it. Then, in the block named `conn` we run a computation which depends
upon a "foreign" reference which was imported from the surrounding
environment. Since we know how `runUfIntMap` works it's patently clear
to us that neither of those moves could possibly be
meaningful---again, references *only* make sense within the region
between the time a fresh `IntMap` environment is introduced and when
we extract the final result using `evalState`. In other words, the
region defined as the *argument* to `runUfIntMap`.

So let's just document the thing!

~~~
-- | 'Mem' monads allow for the creation and manipulation of mutable
--   references. The use of a reference, reading or writing is valid
--   iff it was created in the same thread. References which escape a
--   'Mem' computation are no longer guaranteed to be meaningful.
--   References imported into a 'Mem' computation will invariably lead
--   to errors or undefined behavior.
class (Monad r, Eq (Ref r)) => Mem r where ...
~~~
{: .language-haskell}

Actually, no, that's terrifically unsatisfying. We're not programming in a
pure, immutable langauge so that we have to deal with ambiguous
notions of "undefined behavior". If it's okay by you, I'll go
implement my Union/Find in C, thankyouverymuch.

## Eliminating bad computations

We'd like to eliminate the ability to write invalid computations of
the forms explored above *statically*. In other words, we need to find
a way to reflect that information in the types of our computations.

If it's not exactly obvious how to achieve that, don't worry. I'll
explore the solution in stages.

### A thread of their own

First, we'd like to begin to track the notion of how two `Mem`
computations which are sequenced together are now *linked* as
belonging to the same region. With this notion we'll maybe be able to
start talking about when two computations are not linked and how they
should not share information.

To do this, we'll add a new type variable to our `UfIntMap`
implementation: a *phantom* type variable just like `v`.
Traditionally, this is called `s` and it's almost an entirely trivial
change. The updated code is below and [here's a diff][the-diff]
showing the changed lines.

[the-diff]:

## Other posts in this series

* [Part 1](http://tel.github.io/2014/07/12/mutable_algorithms_in_immutable_languges_part_1/)
