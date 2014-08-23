---
layout: post
title: Towering Data
comments_enabled: true
---

$$
\newcommand{inl}[1]{\textrm{inl}[#1]}
\newcommand{inr}[1]{\textrm{inr}[#1]}
\newcommand{fold}[1]{\textrm{fold}[#1]}
\newcommand{recur}[1]{\textrm{recur}[#1]}
\newcommand{generate}[1]{\textrm{generate}[#1]}
\newcommand{unfold}[1]{\textrm{unfold}[#1]}
$$

Previous, I stated the [*types of data*][types-of-data]---the
products, sums, and exponentials. These types of data are already
powerful enough to describe many concepts. For instance

[types-of-data]:http://tel.github.io/2014/07/23/types_of_data/

* For any type $$A$$, $$A + 1$$ indicates $$A$$ or its absence---a
  notion of failure
* For any type $$A$$, $$A + 0^A$$ indicates $$A$$'s *determinism*: we
  must either be able to construct an $$A$$ or construct a situation
  which proves $$A$$ is unconstructable
* The type $$1 + 1$$ is often called $$2$$ or even `Boolean`. The way
  of using $$2$$ is a conditional branch or `if`-construct.

However, these types are suffer from a crucial weakness: they are
inefficient. For instance, like we constructed $$2$$ we can construct
a construct a type $$n$$ for any natural number $$n$$ and this type
could represent all of the numbers $$\{0, 1, ... n-1\}$$.
Unfortunately, the "size" of such a type is proportional to
$$n$$[^y-combinator]. Unfortunately, we have no way of handling the
whole class of such types in a uniform fashion.

Unfortunately, we cannot construct a type representing all of the
naturals at once.

[^y-combinator]: If you are familiar with the untyped lambda calculus then you might realize that the Y-combinator (or other fixed point combinators of that ilk) allow you to break this size restriction. You'll find however that while the types of data bear a passing resemblence to the untyped lambda calculus they are fundamentally weaker. If the Y-combinator were to be built using the tools presented so far it would have a type like `(A -> A) -> A` for any choice of `A`. Translating that type to an expression of logic should be a hint, but it's an interesting exercise to try to create some form of data with that type.

---

I'd like to talk about types which arise as consequences of repeated
rules, or, similarly, types which arise as solutions to certain kinds
of type equations. This will enrich our types of data with
*recursion*/*induction*, *corecursion*/*coinduction*, and the general
notion of fixed points.

These types will be efficient. If the core types of data represented
legal moves in the game we're playing then the types I want to talk
about now represent whole *strategies*.

| Type | Haskell | Logic | High-school Algebra |
|-
| $$1$$ | `Unit` | trivial | 1 |
| $$0$$ | `Void` | impossible | 0 |
| $$\_ + \_$$ | `(+)` | or | addition |
| $$\_ \times \_$$ | `(*)` | and | multiplication |
| $$\_ \rightarrow \_$$ | `(->)` | implication | exponentiation |
| $$\mu \_$$ | `Mu` | induction | $$\infty$$ |
| $$\nu \_$$ | `Nu` | coinduction | $$\infty$$ |

At this point, the analogy with high school algebra breaks down. Using
these types we will be able to study many shades of grey of
infinities, but high school only teaches one.

### *Aside:* Type schemata

Previously, I described type connectives like $$\_ + \_$$ as being a
*type schema* with two holes. We *instantiate* the schema by providing
types to fill the holes in with.

In order to describe the types below better we'll need a better syntax
for refering to type schemata. The standard mechanism is to write $$x
. E$$: the $$x$$ is a fresh, local type variable which is available in
any type expression $$E$$. For instance, all of these refer to the
same type schema:

$$
\begin{align}
&1 + \_ \\
x . &1 + x \\
y . &1 + y
\end{align}
$$

In particular, the name of the type variable is immaterial, though it
should be chosen carefully as to avoid confusion.[^type-exponentials]

Types like $$\mu \_$$ and $$\nu \_$$ are not instantiated by filling
the holes with types but instead with type schemata. Some examples of
this syntax follow:

$$
\begin{align}
\mu x &. 1 + x \\
\nu y &. 1 + y \\
\mu x &. \nu y . x + y \\
\mu x &. (\nu y . x + y) \\
\end{align}
$$

Note that the latter two are identical; the parentheses merely clarify
the syntax.

[^type-exponentials]: You might think at this point that these type schema are nothing more than "exponentials" at the type level. Indeed, if they were then we'd have nice notation to avoid this ambiguity at least. For now, however, I will avoid trying to formalize that. In particular, before we can have type level lambdas we would need to (a) formulate a notion of "types of types", (b) formulate the rules that allow for their construction, and (c) formulate the rules that allow for their use. It will turn out to be a *very* hairy endeavor.

### *Aside:* Type Equations and Fixed Points

One way to envision the entire goal of these new types is to try to
extend the basic types of data with solutions to recursive primal[^primal]
systems of type equations. For instance, we might consider the system

$$
\begin{align}
  a &= 1 + b \\
  b &= 1 \times a
\end{align}
$$

and assert that we have extended our types to include whatever $$a$$
and $$b$$ are. This is not well-defined, though, for two reasons:

1. Nothing says that $$a$$ and $$b$$ have to be unique types.
2. Asserting that the types $$a$$ and $$b$$ exist does not give us
   mechanisms for their construction or use!

So, we seek to find solutions to these equations but do so in a way
principled much like the basic types of data were.

[^primal]: By "primal" here I mean that the left-hand side of each equation is purely a type variable and not a more complex expression containing variables. Reduction of more general systems of equations to primal ones is not necessarily obviously possible (in high-school algebra we make use of *subtraction* and *division* to do so, but those don't appear to make much sense in types) and so we'll just ignore it!

The general trick we use to become more principled is to weaken
equality in those equations. For instance, consider the follow
equation (equivalent to the previous example, actually)

$$
\begin{align}
  a &= 1 + a
\end{align}
$$

Instead of positing that some type $$A$$ exists which is equal to
$$1 + A$$, we'll posit that a type $$A$$ exists along with a function
of type $$A \rightarrow 1 + A$$ or that a type $$A$$ exists along with
a function of type $$1 + A \rightarrow A$$. You might, at this point,
be unsurprised when I say that those two tricks are dual.

---

## Recursive and Corecursive Types of Data

With those preliminaries dispatched, let's examine two new
higher-order type connectives.

### Mu, the bricklayer

The higher-order type connective $$\mu \_$$ is called "mu". It cannot
be represented precisely in Haskell nor is it implemented by default.
Its type is constructed by filling the hole with a type
schema[^strictly-positive] which we'll refer to by a variable, $$S$$.

[^strictly-positive]: Technically not any schema will do---it must be "strictly positive" which means that every type hole is in "positive position". Informally, every type hole must be on the right side of exponentials or, more generally, in construction not use. So, $$x . 1 + x$$ is strictly positive, so is $$x . A \rightarrow x$$, but $$x . x \rightarrow A$$ is not. Interestingly $$x . (x \rightarrow A) \rightarrow A$$ *is* strictly positive. Can you see why?

In order to construct $$\mu S$$ you must have a value, say $$e$$, of
$$S \mu S$$. Then, $$\fold{e}$$[^nomenclature] is a value of `Mu s`. This
seems a little circular, surely, and it is. It reflects the fact that
`s` must have a *base case*.

[^nomenclature]: This nomenclature is happily taken from [Practical Foundations for Programming Languages](http://www.amazon.com/Practical-Foundations-Programming-Languages-Professor/dp/1107029570), though the technique is hardly novel to that book. That said, if you are interested in types like this then I recommend PFPL.

For instance, if $$S = x . 1 + x$$ then $$S \mu S = 1 + \mu S$$ and as
$$\inl{\top}$$[^mathy-notation] is a value of $$1 + \mu
S$$[^parametricity] then $$\fold{\inl{\top}}$$ is a value of $$\mu
S$$. Indeed, if we name $$z = \fold{\inl{\top}}$$ and produce a lambda
form $$s = \lambda n \rightarrow \fold{\inr{n}}$$ which has the type
$$\mu S \rightarrow \mu S$$ then we can see that $$\mu x . 1 + x$$ is
the type of Peano Naturals:

$$
\begin{align}
0 &= z \\
1 &= s z \\
2 &= s s z \\
&\dots
\end{align}
$$

Thus, because $$1 + \mu S$$ has values which can be constructed
without first constructing $$\mu S$$, we find our needed base case
$$z$$.

[^mathy-notation]: Sorry about introducing things like $$\inl{\_}$$ and $$\top$$ despite not using them in the previous article. We can't yet refer to actual Haskell notation, but hopefully these values are easy to follow. Breifly: $$1$$ has the value $$\top$$, $$1 \times 1$$ has the value $$\langle \top, \top \rangle$$, $$1 + 1$$ has values $$\inl{\top}$$ and $$\inr{\top}$$, while $$1 \rightarrow 1$$ has the value $$\lambda x . \top$$.

[^parametricity]: Indeed, it's a value of $$1 + A$$ for *any* $$A$$.

## Footnotes
