---
layout: post
title: Let me tell you about the types of data
---

Let me tell you about the types of data. This may be a different
presentation of data than you are familiar with, so let me be clear
about what I mean by *data*.

Data is the building block of meaning and execution. It's complete
description subsumes meaning through *type* and execution by the
methods of producing and consuming data. To be clear, *data*, in the
context of this article, is not binary bits of any form. Those are a
*serialization* and will not be considered further.

It's my hope that if you understand data then you will have a powerful
tool for reasoning about the meaning and behavior of programs. It's
also my hope that you will feel at least some small part of the reason
why I refer to the subject of this article as "the types of data"
instead of "one kind of data" or "data in Haskell". Something large
lurks below the surface here.

# The types of data

The types of data are $$1$$, $$0$$, $$\_ + \_$$, $$\_ \times \_$$, and
$$\_ \rightarrow \_$$ where underscores denote "holes" so that $$1$$
has no holes and $$\_ \times \_$$ has two. They are also called
"unit", "void", "choice", "pair", and "function space" respectively.
In the domain of logic they are called "triviality", "impossibility",
"or", "and", and "implication" respectively. In the domain of
high-school algebra they are called "1", "0", "addition",
"multiplication", and "exponentiation".

| Type | Haskell | Logic | High-school Algebra |
|-
| $$1$$ | `Unit` | trivial | 1 |
| $$0$$ | `Void` | impossible | 0 |
| $$\_ + \_$$ | `(+)` | or | addition |
| $$\_ \times \_$$ | `(*)` | and | multiplication |
| $$\_ \rightarrow \_$$ | `(->)` | implication | exponentiation |

Data is constructed from *connectives* which may or may not be
dependent on other pieces of data. If a connective does not depend
upon any other data then it is a constant. Let's talk about the
constants first then the connectives.

## Unit, the useless type

The type $$1$$ is called "unit". In default Haskell it's denoted by
`()`. Unit is special because we can *construct* it without any
restriction whatsoever. We can demonstrate this using Haskell. For
instance, first we *define* a type named `Unit` (don't worry if you
don't understand this syntax)

~~~
data Unit = T deriving Show
~~~
{: .language-haskell}

This syntax indicates that we have introduced a new type named `Unit`
which can be constucted in exactly one way, by invoking `T`. This
implies we can create `Unit` whenever we want by using `T`, which is
just what we wanted. The `deriving Show` bit is unimportant, merely
needed so that we can more conveniently interact with `Unit` using the
interpreter.

~~~
>>> T
T
it :: Unit
~~~

For those unfamiliar with Haskell's interactive prompt, GHCi, this
indicates that when I typed `T` to construct a value (line 1), I get
the value which can be shown as `T` back (line 2, this is where the
`deriving Show` is used), and then that the most recent value executed
in GHCi, automatically named `it`, has type `Unit` (line 3, the syntax
`x :: t` is read "`x` has type `t`").

Now that we've talked about the way we can *consider* the `Unit` type
and the way that we can *create* `Unit`-typed values we must examine
how we *use* `Unit`-typed values. In particular, assume that I have
given you a value of type `Unit`: what can you now do?

~~~
val :: Unit
~~~

Since you can construct new values of `Unit` whenever we like without
restriction, me providing you gives you *nothing new*. So, we can
conclude that there is *no* way to use a value of type `Unit`. It is
inert.

To summarize: the type $$1$$, called `Unit`, is a type constant which
can be constructed trivially and in exactly one way, called `T`. It is
useless.

As a side note, notice that `Unit` has exactly one inhabitant, `T`
[^totality]. This is why it's called "1" and why it behaves like the number 1.

[^totality]: In Haskell, honestly, this is untrue for technical reasons. This is a general theme, unfortunately, in that the types of data as I describe them are only "mostly" properly implemented in Haskell. Thus, representing them in Haskell is a matter of convenience alone. Their meaning and structure as described is the heart of the matter.

## Void, the unobtainable

The type $$0$$ is called "void". It does not exist in default Haskell
but is available in the package
[`void`](http://hackage.haskell.org/package/void) and there called
`Void`. Void is special because we *cannot construct it* for any
reason whatsoever. Anyone who claims to have a value of type $$0$$ is
a liar. Let's define `Void` in Haskell to demonstrate.

~~~
data Void
~~~
{: .language-haskell}

Unlike the definition of `Unit`, there are no constructors for `Void`.
Thus, we cannot construct it. It's not even possible to demonstrate
this fact in the interpreter. The very notion is senseless.

Up until this point `Void` might appear to be very useless, but,
interestingly `Void` is actually very useful![^unit-is-useless] Again,
assume that I have given you a value of type `Void`: what can you now
do?

[^unit-is-useless]: Remember that it's `Unit` that is useless. We can even think about this exactly: `Unit` has no uses. On the other hand, `Void` has, in some punning sense, *all* uses. So `Unit` is useless and `Void` is merely unobtainable.

~~~
val :: Void
~~~

Oddly, as I said just before, anyone who claims to have a value of
type `Void` is a liar. So I was a liar, and now so are you. Thus, if I
provide you a type `Void` then you can feel free to lie back to me and
claim that you have now *anything at all*. What I'd like you to see is
that `Void` is a *bomb*.

Another way of saying this is that since `Void` can never be
constructed then anything which happens "after" a value of `Void` has
been produced is entirely hypothetical and thus no longer required to
follow the laws of the universe.

This is an odd concept, and we'll revisit it later. To foreshadow,
however, if you are familiar with the notion of proof by contradiction
then you are familiar with `Void`.

So, again, to summarize: the type $$0$$, called `Void`, is a type
constant which cannot be constructed. It is infinitely useful.

As a side note, notice that `Void` has exactly zero inhabitants. This
is why it's called "0" and why it behaves like the number 0.

## Aside: Whispers of duality

After having now introduced $$1$$ and $$0$$ I can spend a moment
talking about an important component of the nature of the types of
data, namely: *duality*. Duality is a powerful concept throughout the
laws of math and physics. It is an observation: oftentimes, concepts
come in pairs which behave identically, but as *mirror images*.

If you are familiar with the design and function of circuits then you
are already familiar with
[a number of dualities](http://en.wikipedia.org/wiki/Duality_(electrical_circuits)).

$$1$$ and $$0$$ are dual to one another, flipped over the axis of
construction v. use. $$1$$ is trivially constructable but useless
while $$0$$ is impossible to construct but infinitely useful. We will
see more duality going forward.

Duality is not universal---it may appear or it may not. That said,
observations of duality tend to indicate the robustness and
generalizability of a concept.

## Sums, choices of construction

The type *connective* $$\_ + \_$$ is called "sum". It exists by
default in Haskell
[named `Either`](http://hackage.haskell.org/package/base-4.7.0.1/docs/Prelude.html#t:Either)
but we will reimplement it below. Sums are connectives, not constants,
and thus have holes. In general, we must *fill* these holes with other
types in order to have a complete type. It may be simpler to think of
$$\_ + \_$$ as a *type schema* and $$A + B$$ as a type. Sometimes you
could say *fully-saturated type* to indicate that all holes have been
filled.

In Haskell we indicate the use of holes via *type variables*. To
demonstrate, here is a definition of $$\_ + \_$$ in Haskell

~~~
data a + b = Inl a | Inr b deriving Show
~~~
{: .language-haskell}

This syntax indicates that the type operator (i.e. type connective)
named `(+)` has two holes named `a` and `b`. It can be construct in
one of two ways: either using the name `Inl` (for "inject left") or
the name `Inr` (for "inject right"). In order to construct a value of
`a + b` with `Inl` we must also have a value of type `a`. In order to
construct a value of type `a + b` with `Inr` we must also have a value
of type `b`. Again we use `deriving Show` for convenience.

So long as it is constructable can demonstrate `a + b` interactively,
but we must decide upon what `a` and `b` will be. For instance, we can
construct values of type `Unit + Unit` in two ways

~~~
>>> Inl T :: Unit + Unit
Inl T
it :: Unit + Unit
>>> Inr T :: Unit + Unit
Inr T
it :: Unit + Unit
~~~

We can even construct the left-side value of type `Unit + Void` using `Inl`[^showing-void]

~~~
>>> Inl T :: Unit + Void
Inl T
it :: Unit + Void
~~~

[^showing-void]: This code won't work by itself. The code which knows how to show `a + b` assumes we can show both `a` and `b`. So far we haven't explained how to show `Void` and we can't use `deriving Show`. Of course, "showing `Void`" doesn't make sense because there's never anything to show, so we just have to tell the compiler this fact: `instance Show Void where show _ = error "impossible!"`

but we cannot produce the right-side value using `Inr` since it would
demand we produce a value of type `Void` which is impossible. Further,
we cannot construct any values of type `Void + Void` as both the left
and right constructors are blocked: `Void + Void` is just as
unobtainable as `Void` is.

Now that we've talked about the way we can consider the type `a + b`
and the way we can construct it we must examine how we use values of
type `a + b`. Assume I give you a value of type `a + b` and further
assume that you know a way to *use* values of type `a`. Importantly,
we have not yet assumed you know how to use values of type `b`.

In this circumstance, you are stuck if it turns out that the value I
gave you was of the form `Inr something`: since `something :: b` and
you don't know how to use `b`, you don't know how to use `a + b`.

So we can conclude that in order to use a value of type `a + b` we
must know how to use *both* `a` and `b`.

To summarize: the type $$a + b$$, called `a + b` or the sum of `a` and
`b`, is a type built from the $$+$$ connective which can be
constructed from either a value of type `a` or a value of type `b`. In
order to use such a value you must have both a way to use `a` and a
way to use `b` so that you can handle either case.

As a side note, notice that the number of inhabitants of `a + b` is
equal to the number of inhabitants of `a` plus the number of
inhabitants of `b`. This is why it's called "sum" and how it behaves
like algebraic addition.

## Products, collections of constructions

The type connective $$\_ \times \_$$ is called "product" and has two
holes. It exists by default in Haskell and is called
[a pair or tuple](http://hackage.haskell.org/package/base-4.7.0.1/docs/Data-Tuple.html)
but we will reimplement it below.

~~~
data a * b = Tuple a b deriving Show
~~~
{: .language-haskell}

This syntax indicates that values of type `a * b` can be constructed
in one way: by using the `Tuple` constructor on a value of type `a`
and a value of type `b` together. Again, `deriving Show` is
inessential but convenient.

Some example interactive product constructions follow

~~~
>>> Tuple T T :: Unit * Unit
Tuple T T
it :: Unit * Unit
>>> Tuple T (Inl T) :: Unit * (Unit + Void)
Tuple T (Inl T)
it :: Unit * (Unit + Void) 
~~~

Notably, we cannot construct values of `a * Void`, `Void * b` or
`Void * Void` for any types `a` or `b`.

Now that we've talked about the way that we can consider the type
`a * b` and the way we can construct it we must examine how we use
these values. Assume I give you a value of type `a * b`. If you knew
how to use values of type `a` then you could merely extract the `a`
and use it. Likewise, if you knew how to use values of type `b` then
you could extract it and use it. So we can conclude that in order to
use a value of type `a * b` you merely have to have a use for either
`a` or `b`.[^contraction]

[^contraction]: A wary reader might note that you could also get by if you knew how to use *both* `a` and `b` together. This is certainly not eliminated as a possibility, but I'm wording it to be suggestive of duality.

To summarize: the type $$a \times b$$, called `a * b` or the product
of `a` and `b`, is a type built from the $$\times$$ connective and is
constructed from a value of `a` and a value of `b` together. In order
to use such a value you only need a way to either use `a` or use `b`.

As a side note, notice that the number of inhabitants of `a * b` is
equal to the number of inhabitants of `a` times the number of
inhabitants of `b`. This is why it's called "product" and how it
behaves like algebraic multiplication.

## Aside: Further duality

It should be clear by this point that products and sums dual along the
construction v. use axis just like $$1$$ and $$0$$ were, but let's be
explicit about it anyway.

* Sums are constructed from values of *either* their left type or
  their right and used when there is a use for *both* the left and the
  right.
* Products are constructed from values of *both* their left type and
  their right type together and used when there is a use for *either*
  values of their left type or their right type.

This duality is interesting because it suggests that if we could
somehow package up the notion of "use" into a type then we would have
a property like "uses of products are sums of uses" and "uses of sums
are products of uses" showing that the two types of data *fit*
together very nicely.

## Aside: Multi-way products and sums

It might feel restrictive that products and sums as introduced only
allow two "arguments" each. Of course, we can extend these notions by
noticing that, e.g. `a * (b * (c * d))` behaves like a four-way
product and `a + (b + (c + d))` behaves like a four-way sum. There is
some technical detail here about whether `a * (b * c)` is *the same
as* `(a * b) * c` or merely "interchangeable with", but that's not
worth examining.

More interestingly, multi-way sums and products open space to ask the
question of what a 0-way product or sum is. We can use the pattern to
investigate these types.

A 0-way product is a type which can be introduced using nothing at all
and has no interesting uses (what is the 0-way notion of using "either
`a` or `b`" for using `a * b`?). In other words it is `Unit`.

A 0-way sum is a type which has no introduction form and can be used
toward any end whatsoever. In other words it is `Void`.

This generalization is not much more than a thought
experiment---there's no reason to force it if it feels uncomfortable.
Instead, it mere is suggestive of the importance of `Unit` and `Void`.
It's also suggestive of the power of construction v. use duality.

## Exponentials, implications, hypotheticals, functions

The type connective $$\_ \rightarrow \_$$ is called "function space".
It exists by default in Haskell, denoted as `a -> b`, and we won't
reimplement it here.

We construct values of `a -> b` by showing that we can *use* `a` to
*construct* `b`. Function spaces are thus *very important* as they
link use to construction. We demonstrate this construction with
*lambda forms* or anonymous functions with syntax like $$\lambda x
\rightarrow E$$. In this case, $$x$$ names a value of type `a` which
we are assuming we can construct. Then $$E$$ is a expression
demonstrating our method of using $$x$$ to construct a value of type
`b`. Thus, we can read $$\lambda x \rightarrow E$$ as "assuming a
value of type `a` named $$x$$ we can construct a value of type `b` in
the way described by $$E$$.

An interactive example would be `Unit -> Unit`, i.e. constructing
`Unit` from `Unit`

~~~
>>> :t (\x -> T) :: Unit -> Unit
(\x -> T) :: Unit -> Unit :: Unit -> Unit
~~~

Notably, we have to use `:t` to ask for the type of `(\x -> T)`
because it is not possible to instantiate `Show` for function spaces.

The Haskell syntax above uses `\\` to stand in for $$\lambda$$ since
they sort of look similar. We also must use the parentheses in order
to have `(::)` denote the whole lambda expression.

Another interactive example of more interest might be `a * (b * c) ->
(a * b) * c`

~~~
>>> :t (\(Tuple a (Tuple b c)) -> Tuple (Tuple a b) c) :: a * (b * c) -> (a * b) * c
(\(Tuple a (Tuple b c)) -> Tuple (Tuple a b) c) :: a * (b * c) -> (a * b) * c
  :: (a * (b * c)) -> (a * b) * c
~~~

where we've replaced the standard `x` name with a *pattern match*
indicating the usage of our product. Interpreting pattern matches as
*use* of a value is an important concept in Haskell, but is merely a
detail of our representation of types of data in Haskell. Any way of
using tuples as described previously would suffice.

Now that we've talked about the way that we can consider the type `a
-> b` and the way that we can construct it we must examine how we use
these values. Assume I gave you a value of type `a -> b`. If you
*also* have a value of type `a` then you can fufill the hypothetical:
since `a -> b` is a mechanism for using `a` to produce `b`, you can
execute that specific use of `a` and retreive a `b`. Function spaces
are crystalized *use*.

So to summarize: the type $$a \rightarrow b$$, called the function
space from `a` to `b`, is a type built from the $$\rightarrow$$
connective. It's constructed via lambda forms which represent
hypothetical arguments for how to construct values of `b` using values
of `a`. In order to use such a value, we also require an `a` and then
we execute the argument the `a -> b` type value represents in order to
produce a `b`.

As a side note, notice that the number of inhabitants of `a -> b` is
equal to the number of inhabitants of `b` *raised to the power of* the
number of inhabitants of `a`. This is probably the must unexpected
counting argument thus encountered, but we can produce examples
easily. For instance, let's call the type $$1 + 1$$ by the name $$2$$
and $$1 + (1 + 1)$$ by the name $$3$$. Now, $$3 \rightarrow 2$$ is the
type of ways of converting values of $$3$$ to values of $$2$$. In
order to use $$3$$ we must decide, independently, what to do with its
three values: `Inl T`, `Inr (Inl T)` and `Inr (Inr T)`. In order to
construct a value of $$2$$ we must decide whether to pick `Inl T` or
`Inr T`. This means all together we have to make a binary choice for
three options in each value of $$3 \rightarrow 2$$ which leads to
$$2^3 = 8$$ options.

This is why $$a \rightarrow b$$ is sometimes called an "exponential"
and is sometimes written $$b^a$$.

## Aside: Formalizing *use* and *construction*

Now that we have function spaces we can bring to fruition an idea from
the previous aside. If we decide that our goal is to produce a value
of some type `r` (for "result") then a function `a -> r` is a "use" of
`a`. So, now, we can state that "a use of `a * b` is `(a -> r) + (b ->
r)` and a use of `a + b` is `(a -> r) * (b -> r)`.[^adjunction]

[^adjunction]: Here we can be more clear and note that I only claimed that `(a -> r) + (b -> r)` is *a* use of `a * b`. We can generate *all* uses of `a * b` by noting the equivalence of `a * b -> r`, `a -> (b -> r)`, and `b -> (a -> r)`. This is called the curry [adjunction](http://en.wikipedia.org/wiki/Adjoint_functors) between products and exponentials.

We could also do the opposite and fix a type `s` (for "source") which
we think of as the heart of all values. Now we can say that a function
`s -> a` is a "construction" of `a`. Now we can say that

~~~
(s -> (a * b)) * ((a -> r) + (b -> r))   ->   (s -> r)
~~~

and

~~~
(s -> (a + b)) * ((a -> r) * (b -> r))   ->   (s -> r)
~~~

to completely formalize how products and sums eliminate one another.
This is a bit less pretty to write down, but emphasizes the duality
between construction and use.

## Aside: Explosions, negations, and *ex falso quodlibet*

Now that we have exponentials we can talk more concretely about what
the meaning of `Void` is. As stated previously, if we have a value of
type void then we can produce anything at all. If we represent this
mode of use in terms of a function space then it means that for any
type `a` we might desire, we can automatically construct the function
`Void -> a`. This family of functions, one for every type `a`, is
sometimes known as the *principle of explosion* or by the Latin phrase
"ex falso quodlibet", "from falsehood follows anything".

Since it's impossible to produce a value of type `Void`, we know that
for any type `a` at least one of the following is impossible to
produce as well, values of the type `a` or values of the type `a ->
Void`. If it were not the case, if we could construct both some `a`
and the function `a -> Void` then we could combine them to produce a
value of `Void`. So this must not be possible.

This allows us to extend the "impossibility of `Void`" to any type we
please. Construction of a function `a -> Void` implies that `a` is not
constructable. This is effectively logical negation.

Finally, given the notion of negation we might try to formalize proof
by contradiction. This would read something like "to prove `a` we
assume not `a` and derive impossibility" or, formally

    ((a -> Void) -> Void) -> a

If you try to construct a lambda term to represent this, you will
fail. This drives a division between the notion of "proof" and the
notion of "construction". Our data types talk about how to construct
things, but sometimes we admit proofs without construction.

One branch of logic, intuitionistic logic, tries to cope without the
ability to prove without constrution. Thus, in intuitionistic logic
the structure of data is better preserved... but one must also do
without proof by contradiction which can be painful.

## Conclusion

We've now covered the types of data, $$1$$, $$0$$, $$\_ + \_$$, $$\_
\times \_$$, and $$\_ \rightarrow \_$$. We've seen that we can
describe each one by talking about the shape of its name (i.e. by
describing how to saturate it), how to construct a value of one, and
how to use a value once constructed. Through this analysis we've seen
that we can uncover a very powerful notion of construction/use duality
which is carried by the natural fit between sums and products and
embodied in the left and right sides of our function spaces.

While the types of data start very humbly, we can already construct
now very interesting values. For instance, the type $$2 = 1 + 1$$
introduced above is actually the type of booleans: let `Inl T` be
truth and `Inr T` be falsity.

Unfortunately, while the basis laid so far is very expressive, it is
not yet expressive enough to represent most types that are reasonably
useful in a program. It does not yet even describe all of the types of
Haskell. For this we must introduce one or two further connectives
named $$\mu$$ and $$\nu$$. A further article may explain their
meaning, construction, and use.

But for now I believe this article has served its purpose. These are
the types of data, more or less. They are simple yet brimming with
meaning and a very powerful notion of *fit*.

May they serve you well.

## Discussion

There has been some great discussion of this post which lead to a number of correcting on Reddit. In particular, please take a look at [`/r/haskell`](http://www.reddit.com/r/haskell/comments/2bj7it/let_me_tell_you_about_the_types_of_data/), [`/r/programming`](http://www.reddit.com/r/programming/comments/2bk1cd/let_me_tell_you_about_the_types_of_data/), and [`/r/compsci`](http://www.reddit.com/r/compsci/comments/2bkgic/let_me_tell_you_about_the_types_of_data_xpost/).

## Bibliography

* Per Martin-Löf.
  [On the meannings of the logical constants and the justifications of the logical laws](http://www.ae-info.org/attach/User/Martin-L%C3%B6f_Per/OtherInformation/article.pdf). (1996)

## Footnotes
