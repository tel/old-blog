---
layout: post
title: Analyzing Transducers
comments_enabled: true
---

*In which I explore some small representation changes to the new
 transducers functionality released by Cognitect to make them behave
 "more naturally" from a Haskell point-of-view and demonstrate strong
 theoretical reason for working the way they do.*

*It's a little long! Hopefully, it's information-packed, though. The
 TL;DR might be something like: Transducers are "just" Church-encoded
 list transformers with some extra behavior around end-of-list
 handling.*

The folks at cognitect [just announced][announce] some fairly
interesting new functionality in Clojure/core which they call
*transducers*. Briefly, they describe how many operations on
lists/sets/enumerables/containers/what-have-you are well-abstracted by
considering operations on them in the following form:

~~~
type Transducer r a b = (r -> a -> r) -> (r -> b -> r)
~~~
{: .language-haskell}

[announce]:http://blog.cognitect.com/blog/2014/8/6/transducers-are-coming

This kind of thing is like candy for a Haskeller since it's amenable
to analysis via algebraic data types, so I thought I'd give it a spin.

---

We can examine the direct Clojure source code and translate it to
Haskell for analysis:

~~~
(def map
    ([f]
    (fn [f1]
      (fn
        ([result input]
           (f1 result (f input)))
        ([result input & inputs]
           (f1 result (apply f input inputs)))))))

(def filter
    ([pred]
    (fn [f1]
      (fn
        ([result input]
           (if (pred input)
             (f1 result input)
             result))))))

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

The translations of `map` and `filter` are straightforward

~~~
zmap f f1 result input = f1 result (f input)

zfilt p f1 result input = if p input then f1 result input else result
~~~
{: .language-haskell}

However, the translation of `take` uses an atom and a sentinel value.
We'll ignore the atom for now (as it cannot be done purely) and focus
on the sentinel value. This breaks `take` but still exposes some
interesting structure. Maybe we can return later to fix `take`.

~~~
ztake n f1 = run n where
  run n result input =
    let n'     = n - 1
        result = if n >= 0 then f1 result input else result
    in if n' == 0 then Nothing else Just result
~~~
{: .language-haskell}

Since goal here was merely a faithful translation of the source of
some major Clojure transducer arrows there's not a whole lot of
clarity yet. From here, we'll dive further by using Haskell's type
system to analyze these functions.

## Inference on foreign code

The Haskell code I've written above compiles as is. This might feel
strange given that Haskell is statically typed and there isn't a type
of interest in all of the above but Haskell's *inferencer* is more
than powerful enough to do the job. It also helps that this code is
highly polymorphic which, as we'll eventually see, is basically the
point.

In any case, we can use GHCi's `:type` command to get some information
about these functions.

~~~
map    :: (t3 -> t2) -> (t1 -> t2 -> t) -> t1 -> t3 -> t
filter :: (t1 -> Bool) -> (t -> t1 -> t) -> t -> t1 -> t
take   :: (Ord a1, Num a1) => a1 -> (a -> t -> a) -> t1 -> t -> Maybe a
~~~
{: .language-haskell}

Kind of burns the eyes doesn't it? The inferencer always produces the
*most general type* which, in this case, is too general. We'll apply a
little domain knowledge to winnow down these types and make them more
intelligable.

1. `take`'s first argument ought to be an `Int`
2. We're interested in types like `(r -> a -> r)` so we'll use
   parentheses to highlight where they show up
3. And we'll note that `map`'s type is a bit more general than we
   need, it has something like `(ri -> a -> ro) -> (ri -> b -> ro)`
   but we can unify `ri` and `ro`
4. Finally, we'll just happily rename type variables until they're a
   bit more human-readable

~~~
map    :: (a -> b)    -> (r -> b -> r) -> (r -> a -> r)
filter :: (a -> Bool) -> (r -> a -> r) -> (r -> a -> r)
take   :: Int         -> (r -> a -> r) -> (r -> a -> Maybe r)
~~~
{: .language-haskell}

Now it's much more clear that we have a repeated type, so let's give
it a name

~~~
type Red r a = (r -> a -> r)

map    :: (a -> b)    -> Red r b -> Red r a
filter :: (a -> Bool) -> Red r a -> Red r a
take   :: Int         -> Red r a -> (r -> a -> Maybe r)
~~~
{: .language-haskell}

Ah! Much nicer. These types begin to really talk about how these
functions work---they should fit our intuition. For instance all of
`map f`, `filter p` and `take n` are types "like" `Red r a -> Red r b`
(excepting `take` which we'll fix shortly) which is what the Cognitect
folks call a transducer.

These types also indicate a few strange pieces. For instance, `map`
appears to apply its function "backward". That backwardness will stick
around for a while, but we'll eventually be able to explain
it.[^contravariance]

[^contravariance]: One hint to the backwardness is that the `a` parameter in `Red r a` occurs in an argument or "contravariant" position in the type. This induces a lot of the flipping.

## I thought transducers had to do with lists

Well, they do! It's just quite unobvious. I hope that by the end of
this article the connection will be clear. For now, however, let's
emphasize how much they *don't* have to do with lists!

I mean, they really don't mention any kind of list constructor or data
type at all. Every piece of a transducer is a function!

I think this is a big part of what has Cognitect so excited. If
transducers don't apply to lists directly at all then they
automatically achieve two benefits

1. They can be equally applied to all kinds of "not quite list-like"
   things
2. They will not *construct* any data until they are somehow executed.
   This provides a sort of implicit "thunking" or laziness

What's also interesting is that transducers are not merely the `Red`
type but instead *arrows between `Red`s*. We'll see how this plays out
later.

## Getting some certainty around `Maybe`

So far `take` is a bit of an outlier. Its type does not reflect the
same `Red`ucer-transformer structure as the others due to the `Maybe`
required to match the sentinel value used in the Clojure
implementation.

Here's a sneaky way to get rid of that.

If we think of the type `(r -> a -> r)` as a "reduction frozen in
time" or the "continuation of a reduction" over lists, then we can
interpret `r` as the "result" type and this continuation combines a
"current" result with a new value from our list of type `a` to get the
"future" result.

What happens if we don't have an `a` type? Well, we can't just *stop*.
We need *failure continuation* which will be of type `r`. Let's update
`Red` with that

~~~
type Red r a = (r -> a -> r, r)
~~~
{: .language-haskell}

We'll also need to update the implementations of `map` and `filter`.
Since these functions operate nicely on *infinite* lists they have no
need to even consider this failure continuation we added.

~~~
map :: (b -> a) -> Red r a -> Red r b
map f (f1, z1) = (cont, z1) where
  cont result input = f1 result (f input)

filt :: (a -> Bool) -> Red r a -> Red r a
filt p (f1, z1) = (cont, z1) where
  cont result input | p input   = f1 result input
                    | otherwise = result
~~~
{: .language-haskell}

The interesting behavior is in `take` where we replace the `Nothing`
and `Just` values standing-in for the "end of list" sentinel with our
failure/end-of-list continuation.

~~~
take :: Int -> Red r a -> Red r a
take n (f1, z1) = (cont n, z1) where
  cont n result input =
    let n'     = n - 1
        result = if n >= 0 then f1 result input else result
    in if n' == 0 then z1 else result
~~~
{: .language-haskell}

Finally, now that the types are aligned for *all three* of these
combinators, we can introduce a new alias for transducers!

~~~
type Trans r a b = Red r a -> Red r b

map  :: (b -> a)    -> Trans r a b
filt :: (a -> Bool) -> Trans r a a
take :: Int         -> Trans r a a
~~~
{: .language-haskell}

This looks about like what we'd expect now!

## Transducer composition

One interesting part of transducers so far is this *backwards* notion
we see in `map`.

~~~
map :: (a -> b) -> Trans r b a
~~~
{: .language-haskell}

It might seem a little disconcerting, but, if you allow me to skip
into the future and pull back[^playing-along]

~~~
-- note this is backwards again!
sequence :: Trans [a] a b -> [b] -> [a]
~~~
{: .language-haskell}

we can use it to see that everything actually works the way we'd
expect: we don't somehow *invert* our mapped function

~~~
>>> sequence (map (+1)) [1..10]
[2,3,4,5,6,7,8,9,10,11]
~~~
{: .no-highlight}

[^playing-along]: For those playing along at home, here's how you can implement a version of `sequence` that works for this example: `ssequence xform bs = let (cons, nil) = xform (flip (:), []) in foldr (flip cons) nil bs`. We'll explore it in more depth later.

Instead, what's happening here can be revealed by considering
*composition* of transducers. This is one of the big selling points of
transducers as a whole---they compose trivially using function
composition alone. Let's try it!

~~~
>>> sequence (map (*2) . map (+1)) [1..10]
[3,5,7,9,11,13,15,17,19,21]
~~~
{: .no-highlight}

Huh. I might have expected `[4,6,8,10,12,14,16,18,20,22]` given the
normal intuition I have about composition.

What's apparentl happening here is that *transducers compose
backwards*! Or "backwards" because who's to say what the right order
of composition is.

## Aside: reversing composition

Normally, we think of composition like this: given `f :: b -> c` and
`g :: a -> b` we have

~~~
f . g :: (a -> c)
~~~
{: .no-highlight}

There's a folk trick in the Haskell community, though, that we can
*reverse* this composition if we're willing to introduce a little more
complexity. It looks like this

~~~
(. g) . (. f) :: (c -> r) -> (a -> r)
~~~
{: .no-highlight}

Scary right! Let's unpack it slowly.

1. We've transmogrified both `g` and `f` into `(. g)` and `(. f)`.
   Ignore what that means for now, just noting that it's the same
   transformation to each.
2. We're composing the transmogrified versions "backwards" now and,
   indeed, if we tried to go the other way it wouldn't typecheck
3. The result used to be `(a -> c)` but it's now somewhat backwards
   itself---the `c` part comes before the `a` and each has been
   augmented with another one of those annoying `r` parameters.

Did we lose anything via this transformation? Let me show you that we
did not!

~~~
>>> let h = (. g) . (. f)
>>> :t h
h :: (c -> r) -> (a -> r)
>>> :t h id
h id :: A -> C
~~~
{: .no-highlight}

In other words, after we've composed a big "reverse chain" of
transmogrified functions we can restore our original goal by simply
applying the `id` function. If we look at the types of `h` and `id`
again

~~~
h  :: (c -> r) -> (a -> r)
id :: x -> x
~~~
{: .no-highlight}

we can see that when we apply `id` to `h` we restrict the `r` type to
be `c` so that the result becomes `a -> c` like we wanted.

So now that you've taken me on faith for a while and we've seen the
properties of this weird transmogrification... what was it?!

Briefly, the `(. f)` transformation is syntax for `(\cont -> cont .
f)`. In other words, we transform a function from its base form to its
continuation-passing style form. Thus, the property explored above is
that "functions compose backwards in CPS form". Finally, we apply the
identity continuation, `id`, to convert things back to base form.

CPS is always weird if you've not seen it before, but hopefully the
*properties* of this trick at least make sense.

Back to our regularly scheduled program now.

## CPS-ing transducers

Let's "fix" transducers by CPSing them so that the compose the right
way 'round. It's as simple as

~~~
-- Z means bizarro! This is the standard abbreviation 
-- marking continuation-passing style

-- Two different result types, sadly! This'll make the types
-- complicated for a short bit

type ZRed r' r a = Red r a -> r'
type ZTrans r' r a b = ZRed r' r a -> ZRed r' r b

zmap :: (a -> b) -> ZTrans r' r a b
zmap f = (. map f)

zfilter :: (a -> Bool) -> ZTrans r' r a a
zfilter p = (. filter p)

ztake :: Int -> ZTrans r' r a a
ztake n = (. take n)
~~~
{: .language-haskell}

And after all that work we can see that composition works "right" now.[^zsequence]

~~~
>>> zsequence (zmap (*2) . zmap (+1)) [1..10]
[4,6,8,10,12,14,16,18,20,22]
~~~
{: .no-highlight}

[^zsequence]: `zsequence` is still "future technology" for now. If you're playing along at home it can be implemented (scarily) as `zsequence :: ZTrans (Red [b] a) [b] a b -> [a] -> [b]` with `zsequence xform as = let (cons, nil) = xform id (flip (:), []) in foldr (flip cons) nil as`. This will get better b the end though! For the curious, however, take a look at what `ZTrans (Red r a) r a b` expands to: `Trans r a a -> Trans r b a`. It's a "transducer transformer".

## "I told you all that so that I could tell you this..."

At this point you're almost certainly wondering why in the world I
would want to introduce this complicated CPS transform! It's certainly
just a big mess. Do we really want proper compositon order that badly?

Well, no. It's a trick. I just wanted this type:

~~~
type ZRed r' r a = (r -> a -> r, r) -> r'
~~~
{: .language-haskell}

In fact, really, I don't even care about the two result types. We're
always headed to the same final result, so let's just unify them. I'll
give it a new name, too.[^unFold]

~~~
{-# LANGUAGE RankNTypes #-}

newtype Fold a = Fold { unFold :: forall r . (r -> a -> r, r) -> r }
~~~
{: .language-haskell}

[^unFold]: If you're not too familiar with Haskell syntax, the `newtype`/`Fold`/`unFold` bit below is sugar for automatically introducing the following functions: `Fold :: (forall r . (r -> a -> r, r) -> r) -> Fold a` and `unFold :: Fold a -> (r -> a -> r, r) -> r`. The `forall` bit just means that the type works for any result type we can imagine. This is basically what we want and will prevent us from having to track `r` types everywhere.

## Aside: Folds are Lists

Despite their scary type, `Fold a` is just `[a]`. Let me show you:

~~~
listFold :: [a] -> Fold a
listFold xs = Fold (spin xs) where
  spin []     (cons, nil) = nil
  spin (a:as) (cons, nil) = cons (spin as (cons, nil)) a

foldList :: Fold a -> [a]
foldList (Fold f) = f (flip (:), [])
~~~
{: .language-haskell}

These two functions, `listFold` and `foldList` map back and forth
between `Fold`s and `[a]`s. They also have the property that `listFold
. foldList = id` and `foldList . listFold = id`. We can see the
`foldList . listFold` direction easily.

~~~
>>> (foldList . listFold) [1..10]
[1,2,3,4,5,6,7,8,9,10]
~~~
{: .no-highlight}

And if we inline `listFold . foldList` it looks a bit like

~~~
listFold (foldList xs)
=
spin xs (flip (:), [])
=
case xs of
  []     -> []
  (a:as) -> a : spin xs (flip (:), [])
~~~
{: .no-highlight}

which you ought to convince yourself is a no-op.

Whenever you have two types with functions which map between them such
that round-trips in both directions are no-ops, you say that those two
types are *isomorphic*. Which is a fancy way of saying that they're
functionally identical because I can always use one of those
transformer functions[^witnesses] to have either a `Fold` or a list,
whichever I need.

[^witnesses]: The standard name for these "transformer functions" is "witness to the isomorphism" or just "witness"

Anyway, now let's rebuild transducers atop `Fold`.

## Transducing Folds

If `Fold` is a lot like `Red` from before, then we ought to be
interested in types like `Trans` too. 

~~~
type FoldArrow a b = Fold a -> Fold b
~~~
{: .language-haskell}

Honestly, though, since we're interested in these arrows more than
`Fold`s themselves let's inline this type and give it a rather fancy
name

~~~
{-# LANGUAGE TypeOperators #-}

newtype (~>) a b = 
  FA { foldArrow :: forall r . ((r -> a -> r, r) -> r) -> (r -> b -> r, r) -> r }
~~~
{: .language-haskell}

So now our task is just to find implementations of types like

~~~
map       :: (a -> b)    -> (a ~> b)
filter    :: (a -> Bool) -> (a ~> a)
take      :: Int         -> (a ~> a)
sequence  :: (a ~> b) -> ([a] -> [b])
transduce :: (a ~> b) -> (r -> a -> r) -> r -> [b] -> r
~~~
{: .language-haskell}

These functions "obviously" exist since we can implement them using
the `Fold a`/`[a]` isomorphism I just mentioned above. I'll show that
first, but it's important to remember that half the benefit of
transducers is that they are representation-free---we'll eventually
want to avoid "going through lists".

First we'll need another isomorphism which we build atop the ones we
already have.

~~~
real :: (a ~> b) -> (Fold a -> Fold b)
real (FA a) f = Fold (a (unFold f))

imag :: (Fold a -> Fold b) -> (a ~> b)
imag f = FA (\un -> unFold (f (Fold (unsafeCoerce un)))) -- see footnote
~~~
{: .language-haskell}

[^unsafe!]: Hopefully you jumped at the sight of this `unsafeCoerce`! It's a cromulent use, though. We know that in the context of a particular `Fold` arrow the `r` type is the same, but Haskell's type system cannot prove it! Oftentimes you get weird problems like this when dealing with existential/universal types like what was introduced with `r`. We'll eliminate `real`/`imag` before too long, though.

Immediately we get `sequence` and `scramble` witnessing the
isomorphism between `Fold` arrows and `List` arrows like `[a] -> [b]`

~~~
sequence :: (a ~> b) -> ([a] -> [b])
sequence f = foldList . real f . listFold

scramble :: ([a] -> [b]) -> (a ~> b)
scramble f = imag (listFold . f . foldList)
~~~
{: .language-haskell}

From here the rest of what we're looking for come immediately from
Haskell built-ins (namespaced by `Prelude`)

~~~
map :: (a -> b) -> (a ~> b)
map f = scramble (Prelude.map f)

filter :: (a -> Bool) -> (a ~> a)
filter p = scramble (Prelude.filter p)

take :: Int -> (a ~> a)
take n = scramble (Prelude.take n)
~~~
{: .language-haskell}

Finally, if we use the `Category` class to overload composition via
`(.)` we'll get something that looks really familiar

~~~
import Control.Category

instance Category (~>) where
  id = FA id
~~~
{: .language-haskell}

~~~
>>> sequence (map (*2) . map (+1)) [1..10]
[4,6,8,10,12,14,16,18,20,22]
~~~
{: .no-highlight}

Beautiful!

## Going the hard way

So the final step is to eliminate use of lists. Lists appear deep
inside the `scramble` and `sequence` functions where I build the
`real`/`imag` isomorphism via the `foldList`/`listFold` isomorphism.

I'd like to skip past the entire `Fold` type. Let's try to implement
`sequence` and `scramble` directly then.

### Sequencing the hard way

Let's look at what we need for `sequence`

~~~
sequence :: (a ~> b) -> ([a] -> [b])
sequence (FA f) as = _ f as
~~~
{: .language-haskell}

If you type that code into a file and try to load it with GHCi you'll
get an error message explaining that the `_` has to have a type like

~~~
(((r -> a -> r, r) -> r) -> (r -> b -> r, r) -> r) -> [a] -> [b]
~~~

which is our real goal, stripped of type renaming. Can we achieve
that?
