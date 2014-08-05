---
layout: post
title: Pure Profunctor Lenses
comments_enabled: true
---

Lenses, like `Lens s a`, often get described as a pair of a getter and
a setter like `(s -> a, s -> a -> s)`. Then people factor out the `s`
and write things like `s -> (a, a -> s)` or `s -> Store a s` and start
saying that lenses are costate comonad coalgebras.

Which is great. But what about `Prism`s?

---

Let's start over again. We'd like to abstract out the function type
constructor `(->)`. With no particular justification, let's say that a
"function-like" thing is any type which can be pre- and post-composed
like

~~~
dimap :: Functionlike p => (a -> b) -> (b' -> c) -> p b b'    -> p a c
-- for functions
dimap ::                   (a -> b) -> (b' -> c) -> (b -> b') -> (a -> c)
dimap g h f = h . f . g
~~~
{: .language-haskell}

[This](http://blog.sigfpe.com/2011/07/profunctors-in-haskell.html)
[typeclass](https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/profunctors)
is [well-known](http://hackage.haskell.org/package/profunctors) in
Haskell as the `Profunctor`

~~~
class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
~~~
{: .language-haskell}

I promise we'll get to lenses soon, but let's study `Profunctor`s for
a moment.

## What can you do with a `Profunctor`?

As the section title asks, if I give you a `Profunctor` what can you
do with it?

~~~
data P -- abstract
instance Profunctor P where ...

hereYouGo :: P a b
~~~
{: .language-haskell}

It's somewhat tautological really---*all* we can do is `dimap` it. So,
given two functions `f :: (s -> a)` and `g :: (b -> r)` we can *transform*
`hereYouGo` into `hereWeAre`

~~~
hereWeAre :: P s r
hereWeAre = dimap f g hereYouGo
~~~
{: .language-haskell}

But all that said, what I'd like to focus on is just the first part of
that. Let's give is a provocative name

~~~
{-# LANGUAGE RankNTypes #-}

type Iso a b = forall p . Profunctor p => p b b -> p a a

iso :: (a -> b) -> (b -> a) -> Iso a b
iso = dimap
~~~
{: .language-haskell}

So, great. What did we just build, this `Profunctor`-transformer?

## Profunctor laws

Immediately, one thing our `Iso` does is compose under `(.)`.

~~~
composeIso :: Iso a b -> Iso b c -> Iso a c
composeIso f g = f . g
~~~
{: .language-haskell}

This feels a little magical, but it's really nothing more than
composition of the functions inside of our `Iso`s, the "transformer"
part of our `Profunctor` transformers. Since we've only ever seen one
way to construct `Iso`s, by using `dimap`, we can examine equations
like

~~~
composeIso a b
===                      [ inline composeIso ]
a . b
===                      [ assume a form for a and b ]
dimap g h . dimap f i
===                      [ profunctor law ]
dimap (f . g) (h . i)
~~~
{: .no-highlight}

where the last law comes from the definition of a `Profunctor`
indicating that `dimap` ought to behave nicely with function
composition.

We can use the other law to see that given

~~~
isoId :: Iso a a
isoId = dimap id id
~~~

we have that `isoId == id`.
