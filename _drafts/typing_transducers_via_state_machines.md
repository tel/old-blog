---
layout: post
title: Typing Transducers (as State Machine Transformers)
comments_enabled: true
---

[Previously][typing-transducers-1] I analyzed Clojure's new
transducers feature by considering them as mappings between reducers
of type `forall r . (b -> r -> r) -> (a -> r -> r)` which,
interestingly, showed up as being identical to the Kleisli list arrow
`a -> [b]`.

Unfortunately, this representation lacks a notion of "reduction local"
state and thus cannot capture transformations like `take`. In Clojure
this is not a big problem since you can use Clojure's ambient
mutability to regain state via `atom`s. In Haskell this isn't an
option and we have to seek out other avenues.

[typing-transducers-1]:{{ site.url }}/posts/typing-transducers/

So how can we fix that?

This article attempts to show that *yes, we can* by performing a
slight "upgrade" on the `(r -> a -> r)` reducers converting them to
full state automata. State automata have a really nice structure all
on their own, so this upgrade feels very natural.

## Local state and automata

Ultimately, we'd like to inject a sense of local state into our
reducers. Reducers, as they are, have to transmit all of their local
state via the `r` parameter which is quite limiting.

So let's rip out the reducer in our transducer and replace it by
something with static local state---something like a state machine.

~~~
data Red i o where
  Red :: s -> (s -> i -> s) -> (s -> o) -> Red i o
~~~
{: .language-haskell}

Here we introduce the *existentially quantified* type variable `s` to
represent the hidden internal state of our machine. As it's
existentially quantified it follows two rules:

1. Whoever *constructs* a `Red`ucer is allowed to choose what `s` is.
2. Whoever *uses* a `Red`ucer may be allowed to access a value of `s`
   but is not allowed to know what that type is. They also may not
   return any values of `s` themselves---thus `s` cannot "escape".

These rules clearly favor the constructor of `Red`s. They can choose
whatever kind of internal state they like and any downstream users of
the `Red` can do nothing more than manipulate the internal components
of the `Red` possibly returning an `o`.

Unfortunately, `Red`s by themselves give *too much* power to the
constructor. In particular, since only they know what the internal
state type is they must be the ones to choose the outputs `o`.

## Transducers as automata transformers

What we'd like to do is ask for a type like `forall r . Red i r`.
Let's look at what's required to build such a thing, though:

1. A choice of a type `s`
2. An initial value of `s`
3. A reducer function `s -> i -> s`
4. An output function `forall r . s -> r`

Assuming someone actually did build such a thing, a user could combine
(2) and (4) to get `forall r . r`. If you read about
[The Types of Data][types-of-data] you might recognize what a type
which can be used for anything at all is: it's `Void`/$$0$$ and
represents impossibility.

So `forall r . Red i r` is right out.

[types-of-data]:{{ site.url }}/posts/types_of_data/

Instead, let's bring back the transducers framework and consider
*functions* on `Red`

~~~
newtype (~>) a b = Transducer (forall r . Red b r -> Red a r)
~~~
{: .language-haskell}

This ought to look very familiar! We're back to the old transducer
structure again, polymorphic results and all. The difference now is
that unlike reducers, `Red`ucers have a hidden, existentially
quantified local state variable.

## Aside: `Red`ucers are great

The type I called `Red` is actually a well-loved Haskell type known as
a left `Fold`. These are well-loved because they can be shown to have
all kinds of nice properties.

First up, `Red i` is a `Functor`---by composing transformation
functions on the end of the built-in output function we can transform
the outputs:

~~~
instance Functor (Red i) where
  fmap ab (Red s sis sa) = Red s sis (ab . sa)
~~~
{: .language-haskell}

This makes `fmap` have the type `(a -> b) -> Red i a -> Red i b`. We
can also do a "precompose transform" on the inputs. This sometimes
goes by the name `lmap :: (a -> b) -> Red b o -> Red a o`. If we can
both `fmap` and `lmap` then we've got what's called a `Profunctor`

~~~
instance Profunctor (Red i) where
  rmap = fmap -- fmap is called rmap to parallel lmap
  lmap f (Red s sbs so) = Red s (\s a -> sbs s (f a)) so
~~~
{: .language-haskell}

We can also see that `Red` implements `Comonad` and `Applicative`. The
`Comonad` instance encodes the fact that we can transform a `Red` by
feeding it a finite amount of input. The `Applicative` instance
encodes that we can run `Red`ucers in parallel and combine their
results.

~~~
instance Comonad (Red i) where
  extract (Red s _sis so) = so s
  extend e (Red s sis so) = Red s sis (\s' -> e (Red s' sis so))

instance Applicative (Red i) where
  pure o = Red (const o) const ()

  -- Technically we'd want to use a strict pair for this part
  Red x0 xix xf <*> Red y0 yiy ya =
    Red
        (\s   -> xf (fst s) $ ya (snd s))
        (\s i -> (xix (fst s) i, yiy (snd s) i)
        (x0, y0)
~~~
{: .language-haskell}

## Standard Transducers

Let's see what these new `Transducer`s can get us. We'll warm up on
the standard transducers `tmap` and `tfilt`. In fact, `tmap` is
deceptively simple: if you read the aside then we already wrote it.

~~~
tmap :: (a -> b) -> (a ~> b)
tmap f = Transducer (lmap f)
~~~
{: .language-haskell}

The algebra of transforming `Red`s is rich enough by default that we
can just use the `Profunctor` instance to get `tmap`. That said
`tfilt` will demonstrate the general form for writing `Tranducer`s

~~~
tfilt :: (a -> Bool) -> (a ~> a)
tfilt p = Transducer $ \(Red s sas so) ->
  Red s (\s' a -> if p a then sis s' a else s') so
~~~
{: .language-haskell}

If you're paying attention, the code for `tfilt` is almost completely
identical to the `tfilt` using the previous `(r -> a -> r)` reducers.
This should be unsurprising! `Red` still has an old-style reducer
inside of it... it's just wrapped up with the existential variable as
well.

For that matter, `tflatMap` is equally easy---another direct copy from
the previous form it took.

~~~
tflatMap :: (a -> [b]) -> (a ~> b)
tflatMap f = Transducer $ \(Red s sbs so) ->
  Red s (\s' a -> foldl' sis s' (f a)) so
~~~
{: .language-haskell}
