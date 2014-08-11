---
layout: post
title: Typing Transducers (as Kleisli arrows)
comments_enabled: true
---

*The code relating to this post is available [here][here].*

[here]:https://github.com/tel/tel.github.io/blob/master/public/code/scratch/transducers.hs

There's been a little flurry of activity around analyzing Clojure's
upcoming *transducers* using Haskell types. Right now a top contender
looks a bit like this

~~~
newtype (~>) a b = Transducer (forall r . (b -> r -> r) -> (a -> r -> r))
~~~
{: .language-haskell}

It arises from thinking of transducers as "reducer transformers" where
reducers were values of type `(a -> r -> r)`. We can create a few
basic transducers like `tmap` and `tfilt`, we can compose transducers
using (backwards) function composition, and we can "sequence"
transducers to operate them over lists using `tseq`:

~~~
instance Category (~>) where
  id = Transducer id
  Transducer g . Transducer f = Transducer (f . g)

tmap :: (a -> b) -> (a ~> b)
tmap f = Transducer $ \brr a r -> brr (f a) r

tfilt :: (a -> Bool) -> (a ~> a)
tfilt p = Transducer $ \arr a r -> if p a then arr a r else r

tseq :: (a ~> b) -> [a] -> [b]
tseq (Transducer f) as = go as where
  go []     = []
  go (a:as) = f (:) a (go as)
~~~
{: .language-haskell}

Transducers are nice for being representation independent---only
`tseq` actually uses the list data type. This gives them nice laziness
properties and allows transformation between containers of various
types.

Given that reducers operate a bit like functions on lists and
transducers are mappings between reducers, it sort of feels like `a ~>
b` ought to be a bit like `[a] -> [b]` as evidenced by `tseq`.
However, Paolo Capriotti [pointed out][0] that there's a *much* easier
representation of `a ~> b`. Here's the derivation:

~~~
a ~> b
== [ inline ]
forall r. (b -> r -> r) -> (a -> r -> r)
== [ rearrange ]
a -> (forall r . (b -> r -> r) -> r -> r)
== [ universal property of lists ]
a -> [b]
~~~
{: .no-highlight}

[0]: http://www.reddit.com/r/haskell/comments/2d5ael/monoidal_transducers_are_monoid_homomorphisms/cjm9ro5

The last step is a bit of a strange one if you've never seen it
before, but it basically expresses one of the ideas behind reducers:
lists are isomorphic to their own folds. This is sometimes called a
Church-encoding[^Boehm-Berarducci] or a continuation-passing style
transform. Formally, it just says that `[x]` can be converted to and
from a type `forall r . (x -> r -> r) -> r -> r`.

[^Boehm-Berarducci]: Or, really, [a Boehm-Berarducci encoding](http://okmij.org/ftp/tagless-final/course/Boehm-Berarducci.html)

This derivation suggests that values of type `a ~> b` are exactly the
same as functions of type `a -> [b]`. These functions are called
["Kleisli arrows of the List Monad"][kleisli] which is a long name that does
little more than recognize that they can be composed just like
functions `a -> b` by [using the monad structure of list][kleisli-source].

[kleisli]:http://hackage.haskell.org/package/base-4.7.0.1/docs/Control-Arrow.html#t:Kleisli
[kleisli-source]:http://hackage.haskell.org/package/base-4.7.0.1/docs/src/Control-Arrow.html#Kleisli

If we write out the back and forward transformations between `a ~> b`
and `a -> [b]`:

~~~
regular :: (a ~> b) -> (a -> [b])
regular (Transducer f) a = f (:) a []

cps :: (a -> [b]) -> (a ~> b)
cps cont = Transducer $ \brr a r -> foldr brr r (cont a)
~~~
{: .language-haskell}

we can write some very natural definitions of `tmap`, `tfilt`, and `tseq`

~~~
tmap f    = cps (map f    . return)
tfilt p   = cps (filter p . return)

tseq t as = as >>= regular t
~~~
{: .language-haskell}

and even inherit the `Arrow` structure from `Kleisli []` (although I
also give a direct implementation here)

~~~
instance Arrow (~>) where
  arr   = tmap
  -- first = cps . runKleisli . first . Kleisli . regular
  first (Transducer t) = Transducer $ \cdrr (b, d) -> t (\c -> cdrr (c, d)) b
~~~
{: .language-haskell}

This ultimately lets us write some cute transducers using `Arrow`
functions like `(&&&) :: (b ~> c) -> (b ~> c') -> (b ~> (c, c'))`

~~~
>>> let xform = tmap (+1) &&& tmap (+2)
>>> tseq xform [1..4]
[(2,3),(3,4),(4,5),(5,6)]
~~~
{: .no-highlight}

## Flat mapping

Now, in most expositions of transducers around right now the
`tflatMap` function is also given as an example transducer:

~~~
tflatMap :: (a -> [b]) -> (a ~> b)
tflatMap f = Transducer $ \brr a r -> foldr brr r (f a)
~~~
{: .language-haskell}

I deliberately ignored it to begin with because, as it turns out,
`tflatMap` deserves a more important title than merely "another
transducer generator": it's the *canonical* transducer generator and
can be implemented quite nicely in terms of `regular`/`cps`:

~~~
tflatMap = cps
~~~
{: .language-haskell}

## So what?

At this point you may be thinking: *okay, great, the `Transducer` type
is a "Kleisli arrow" thing, so what?* Does this teach us anything
about how to use transducers? Can we think of ways to stretch their
power? Did we learn anything new?

Well, before we go too far we should take the relatively large grain
of salt that we're *assuming* that `a ~> b` is a faithful
representation of Clojure's transducers---[Rich Hickey has claimed that
it is not quite so][rich].

But if we can swallow that then we can see that transducers are
CPS-encoded Kleisli arrows. The CPS encoding means that we're
representation-free and lazy (and gives us the weird backward
composition) while the "Kleisli" bit suggests that we're focused on
the monadic structure of lists.

Which is informative! Lists have rich monadic structure which allows
them to embody non-deterministic search[^non-det]. We also pick up an
`Arrow` instance which might be fun to encode into Clojure taking
advantage of macros to produce some
[`Arrow`-notation][arrow-notation]. This could be a very illuminating
way to construct transducers!

It also suggests some relation to other analyses of transducers such
as interpretation of transducers
[as lensy `Traversal`s][lensy-traversal] or
[`Monoid` homomorphisms][monoid-homomorphisms].

[rich]:http://conscientiousprogrammer.com/blog/2014/08/07/understanding-cloure-transducers-through-types/#comment-1533296409
[arrow-notation]:http://www.haskell.org/arrows/syntax.html
[lensy-traversal]:http://www.reddit.com/r/haskell/comments/2cv6l4/clojures_transducers_are_perverse_lenses/
[monoid-homomorphisms]:http://oleksandrmanzyuk.wordpress.com/2014/08/09/transducers-are-monoid-homomorphisms/

[^non-det]: Non-deterministic transducers ought to be easy to produce, but we don't much see that behavior with the `tmap` and `tfilt` examples. The reason is visible to us when we examine their "clever" definitions via the `cps` function: each of them begins with `return`, the "trivial" Kleisli list arrow which represents no non-determinism whatsoever! We ought to use `regular`, i.e. "flat map" to produce more interesting non-deterministic computations.

## What's missing?

Unfortunately, there's one example use of transducers which totally
escapes this representation: Clojure's `take` transducer. It looks a
bit like this:

~~~
(def take
    ([n]
     (fn [f1]
       (let [na (atom n)]
         (fn
           ([result input]
              (let [n @na
                    nn (swap! na dec)
                    result (if (pos? n)
                             (f1 result input)
                             result)]
                (if (not (pos? nn))
                  (reduced result) ; a terminal value indicating "don't reduce further"
                  result))))))))
~~~
{: .language-clojure}

For those unfamiliar with Clojure, this code closes over one of
Clojure's mutability constructs, an `atom`, to count how many elements
have passed through. This lets the transducer maintain a little local
state and is required to implement `take`. Sadly, `Transducer` is
nowhere near strong enough to represent local state
purely[^taking-is-impossible].

[^taking-is-impossible]: Confidence that this is true comes from analyzing transducers as monoid homomorphisms `Monoid m => (b -> m) -> (a -> m)` as described in [this post][monoid-homomorphisms]. Since `take` isn't a monoid homomorphism, it cannot be encoded in this type. Clojure's mutability breaks the typing guarantees used to make such an assertion and thus allows it. If I want to embed taking into this type I'll need to introduce some kind of local state and generalize the whole thing to be "larger" than just monoid homomorphisms.

My intent isn't to claim that *this* is the reason why `Transducer` is
a poor model for Clojure's transducers---indeed, a completely correct
model embedded in Haskell would suffer this deficiency just because
Haskell cannot have local mutable state like that (unless we embed the
whole computation in a monad).

However, if we beef up what `Transducer`s are then we *can* get a pure
`take`. I'll [explore that a bit][explore] in a later post.

[explore]:https://github.com/tel/tel.github.io/blob/master/public/code/scratch/transducers2.hs

# Footnotes
