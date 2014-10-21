---
layout: post
title: Intro to Call By Push Value
comments_enabled: true
---

*Note* Probably should read
 http://homepages.inf.ed.ac.uk/slindley/papers/frankly-draft-march2014.pdf
 first

If you've ever studied the untyped lambda calculus you've had to
consider there are multiple normalization strategies possible. The two
ones you run into are called [Call By Value (CBV)][levy] and Call By Name
(CBN).[^strict-and-lazy]

[levy]:http://www.cs.bham.ac.uk/~pbl/papers/tlca99.pdf
[^strict-and-lazy]: You might want to mentally translate this to "eager" and "lazy" but I'd encourage you to resist it. CBV and CBN are about the meaning of language while eager and lazy are equally as much about the actual implementation of those meanings.

It's frustrating to find that these two semantics are not identical
(unless we're in a total language). It means that we can't just pick
one and do all of our work there. Instead we always have to do both.

There's no salvation in plain lambda calculus, but Levy's Call By Push
Value framework provides a kind of solution. It's a *larger* or *more
refined* language where both CBV and CBN live happily together. In my
mind, it's strongly hinted at by Filinski's symmetric lambda calculus
from [*Declarative Continuations and Categorical Duality*][dccd], but
much easier on the eyes.

Ultimately, the trick to CBPV is as old as time: we'd like to
distinguish between "things that are" and "things that happen".

[dccd]:http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.43.8729

## Of computations and values

CBPV splits the world into *computations* which merely do and *values*
which merely are. We'll get a flavor for this by introducing two
operators on types $$U$$ and $$F$$.

If we have a computation $$\mathtt{m}$$ of type
$$\underline{A}$$[^underlines] then we *thunk* that computation as
$$\mathtt{thunk\ m} : U\underline{A}$$. If we have a value
$$\mathtt{v}$$ of type $$B$$ then we *produce* the value as
$$\mathtt{produce\ v} : FB$$. It might be weird to think of the notion
of *producing* a value we already have, but oh well.

[^underlines]: Following Levy, computation types are denoted with underlines.

The following paragraph already hints at how progress in computation
arises in CBPV. In particular, $$U$$ indicates "frozen" computation
and $$F$$ indicates "flowing" computation.

To be more clear, we can talk about how we can undo $$F$$ and
$$U$$. Let's say we have a thunk value like $$\mathtt{v} :
U\underline{B}$$. We can pass this around for a while, store it in
variables, replicate it, and then when we wish *force* it like you
might expect

$$
\frac{\mathtt{v} : U\underline{B}}
     {\mathtt{force\ v} : \underline{B}}
$$

What about eliminating the $$F$$ constructor? If we have a computation
like $$\mathtt{m} : FA$$ then we can run and *bind* it. For instance,
if $$E : \underline{B}$$ is a computation with a free variable $$x :
A$$, an *expectation* of a value of type $$A$$, in it then

$$
\frac{\mathtt{m} : FA \ \ \ \ \ \mathtt{E} : \underline{B}}
     {\mathtt{m\ to\ x\ in\ E} : \underline{B}}
$$

So forcing lets us turn values of computations into computations and
computation binding lets us link computations producing values to
computations expecting values.
