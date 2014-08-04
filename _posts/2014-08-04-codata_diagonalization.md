---
layout: post
title: Codata Diagonalization
comments_enabled: true
---

A [recent post by Dick Lipton][post] mentioned Gauss' uncertainty
about Cantor's diagonalization proof as diagonalization involves
assuming the existence of a very strange object---the set of all
decimal expansions of real numbers, say---even if only to prove it
impossible to construct.

[post]:http://rjlipton.wordpress.com/2014/08/03/diagonalization-without-sets/

Gauss protested this, more or less, by saying that infinity was
nothing more than a notational trick referring to a limit and that
conclusions drawn from constructing and manipulating "infinite
objects" were necessarily invalid.

To be clear, this isn't to say that infinities weren't legitate
objects of study, it's merely that they must be *approached* via some
process instead of *obtained*.

More modern mathematics (influenced heavily by recursive and
"infinite" structures in Computer Science) doesn't have a problem with
well-behaving infinite structures---we just recognize that the process
itself can be an object of study.

The point of Lipton's post was to show that we don't need infinite
sets to produce Cantor's argument and can instead do it merely by
describing a certain infinite process.

---

In Haskell we talk about infinite processes like this by talking about
unfolds or codata. Here's the canonical one, I'll call it a
*generator*:

~~~
{-# LANGUAGE GADTs #-}

data Gen f where Gen :: (s -> f s) -> s -> Gen f
~~~
{: .language-haskell}

To understand this type we need to focus on the `(s -> f s)` part
called an `f`-coalgebra. The idea is that if `s` represents some
internal, hidden "state" then `(s -> f s)` is an observer function
allowing us to make `f`-observations of `s`.

For example, here's what happens when `f ~ ((,) a)`

~~~
Gen ((,) a) ~ exists s . (s -> (a, s), s)
~~~
{: .no-highlight}

so if we call the observation function once using the packed-in state
we'll get one observed value `a` and a new internal state. We can do
this `n` times to create a finite list of observations.

~~~
step :: Gen ((,) a) -> (a, Gen ((,) a))
step (Gen obs st) = let (a, st') = obs st in (a, Gen obs st')

observeList :: Int -> Gen ((,) a) -> [a]
observeList 0 _ = []
observeList n gen =
  let (a, gen') = step gen
  in a : observeList (n-1) gen'
~~~
{: .language-haskell}

If you were watching carefully, you probably noticed that the only
reason `observeList` is finite is because it recurses on the first
parameter---the cycle counter. We could, if we so desired, keep
pumping the observation function forever.

So, to summarize, `Gen f` is a (potentially) infinite process
represented by totally finite data. We can use it to represent a
diagonalization argument even Gauss (might have) enjoyed.

## Diagonalizing Codata

~~~
type Stream a = Gen ((,) a)
type Nat = Int
~~~
{: .language-haskell}

The argument goes "Suppose that we have a collection $$G_{1}, G_{2},
\dots$$ of generators of natural numbers..." but we'll translate
"collection" as `Stream`. In other words, we assume only that there's
some process for continually enumerating these generators: a generator
generator.

~~~
gengen :: Stream (Stream Nat)
~~~
{: .language-haskell}

Now, we'll use this `gengen` to produce a new generator (again,
`Stream Nat`) which is provably different from any generator that
`gengen` ever generates. So, ultimately, this proof involves the
creation of a function

~~~
diagonalize :: Stream (Stream Nat) -> Stream Nat
~~~
{: .language-haskell}

The idea will be that we'll slowly consume values from the generators
we generate using the input and then build a result generator which is
always growing too quickly to be in the list of generators we were
given. The result generator always "one-ups" all of the generated
input generators.

~~~
diagonalize :: Stream (Stream Nat) -> Stream Nat
diagonalize g = Gen obs st0 where
  st0 :: (Stream (Stream Nat), Nat)
  st0 = (g, 0)

  obs :: (Stream (Stream Nat), Nat) -> (Nat, (Stream (Stream Nat), Nat))
  obs (gens, max) = (max', (gens', max')) where
    (gen, gens') = step gens
    max'         = spin gen max

  spin :: Stream Nat -> Nat -> Nat
  spin g n = if (m > n) then m+1 else spin g' n where
    (m, g') = step g
~~~
{: .language-haskell}

Examining the pieces here carefully we can see that `st0` is clearly
presented finitely so long as the input generator-of-generators is;
`obs` doesn't do much more than step the generator-of-generators once;
but `spin` is a little clever. It terminates finitely so long as every
input generator eventually produces an output *larger* than some
bound. In other words, `spin g n` returns in a finite amount of time
so long as `g` eventually produces a number larger than `n`. The
easiest way to fix that is to demand that our input generators never
repeat.

So there's the claim. Anyone who claims to have a finite presentation
of a process for creating processes for enumerating all of the natural
numbers, `gengen` must be a liar because `diagonalize gengen` produces
a new generator of natural numbers which `gengen` would never produce.

Is the claim true? Well, `diagonalize gengen` analyzes the input
generators one by one and always (a) skips at least one of their
values (b) outputs a value higher than it ever has before. This means
that every input generator must output at least one number that
`diagonalize gengen` never does. Since `diagonalize gengen` doesn't
`spin` forever it must be a new generator of numbers not in `gengen`.

So there we have it.

## Conclusion

To be completely clear, everything I wrote about here is covered
exactly in Lipton's post. The only novelty I wanted to write about was
the slightly more formal presentation of the argument made available
using `Gen`erators and codata. The Haskell code above is interesting
in that it could compile even in non-Turing complete languages like
Agda so long as you can use the "non-repeating" property of the input
generators to prove that `spin` termiates.
