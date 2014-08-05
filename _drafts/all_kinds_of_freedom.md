---
layout: post
title: All Kinds of Freedom
comments_enabled: true
---

Free structures are a really great way to explore the properties of
some kind of theory. By "theory", I mean a set of *operations* and
*laws*. If you're familiar with Haskell you're familiar with loads of
these---most of the core typeclasses make good examples, `Monad` being
the most loved and hated.

Let's take something simpler, though. The class `Monoid` looks like
this:

~~~
class Monoid a where
  zero :: a
  (<>) :: a -> a -> a
~~~
{: .language-haskell}

(I've taken a little liberty to rename the members of this class. It's
pure aesthetics, though. Everything else is identical.)

So the `Monoid` operations are $$\{\mathtt{zero}, \mathtt{(<>)}\}$$.
You're probably familiar with the laws, too.

~~~
forall a . zero <> a = a   -- left identity
forall a . a <> zero = a   -- right identity

-- and associativity
forall a b c . a <> (b <> c) = (a <> b) <> c
~~~
{: .language-haskell}

Now we'd like to generate the free structure that fits this theory.
Essentially this is the "largest" thing which *models* the theory.
Here's a method for approaching it from the above. First, we translate
the operations to a data type.

~~~
-- I'm using GADTs throughout for aesthetics and the
-- ease of introducing existential type variables.

data FMonoidish a where
  EmbedMonoidish :: a -> FMonoidish a
  Fzero          :: FMonoidish a
  Fplus          :: FMonoidish a -> FMonoidish a -> FMonoidish a

-- so far so good...
class Monoid (FMonoidish a) where
  zero = Fzero
  (<>) = Fplus
~~~

Clearly, this structure satisfies the types of the `Monoid`
operations. Unfortunately, it's *too large* to satisfy the laws. It
actually follows no laws whatsoever---it's the free structure over the
operations of our theory alone (a.k.a. the "signature").

~~~
Fzero <> Fzero    = Fzero  -- (?)
Fplus Fzero Fzero = Fzero  -- (inline (<>))

-- Nope!
~~~
{: .language-haskell}

The problem is, of course, that `FMonoid` exposes the tree structure
of the AST that built it, but the laws of `Monoid` enforce that we
cannot see this structure. So, we need to cut down this type.
