---
layout: post
title: Towering Data
comments_enabled: true
---

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

### *Aside:* Type schema

Previously, I described type connectives like $$\_ + \_$$ as being a
*type schema* with two holes. We *instantiate* the schema by providing
types to fill the holes in with.

The types considered below look similar, like $$\mu \_$$. This is a
weakness in notation however because *this* hole must be instantiated
with a *type schema of a single hole*, not a type.

Thus the following are valid, instantiated types:

$$
\begin{align}
  \mu (1 + \_) && \nu (A \times \_)
\end{align}
$$

This notation has another weakness as well. While the type $$\mu (\_ +
\mu (A \times \_))$$ happens to be clear, nested use of type schema
can be ambiguous in this notation. For instance, $$\mu (A + \mu (\_
\times \_))$$ does not make much sense---there are two ways to
interpret it. In practice, better notation is used, but for the
purposes of this article there will never be any need to write
ambiguous types.[^type-exponentials]

[^type-exponentials]: You might think at this point that these type schema are nothing more than "exponentials" at the type level. Indeed, if they were then we'd have nice notation to avoid this ambiguity at least. For now, however, I will avoid trying to formalize that. In particular, before we can have type level lambdas we would need to (a) formulate a notion of "types of types", (b) formulate the rules that allow for their construction, and (c) formulate the rules that allow for their use. It will turn out to be a *very* hairy endeavor. So we'll just suffice with schemata and bad notation for now.

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
of type $$A -> 1 + A$$ or that a type $$A$$ exists along with a
function of type $$1 + A -> A$$.

---

## Recursive and Corecursive Types of Data

With those preliminaries dispatched, let's examine two new
higher-order type connectives.

### Mu, the bricklayer

The higher-order type connective $$\mu \_$$ is called "mu". It cannot
be represented precisely in Haskell nor is it implemented by default.
The type $$\mu S$$ forms the "least-defined fixed point" of the
equation

## Footnotes
