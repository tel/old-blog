---
layout: post
title: <>
comments_enabled: true
---

I want to talk a bit about the types of fixed points as a natural
extensions of the basic [*types of data*][types of data] arising from
*equations* on types. In programming languages this gives us
*recursion* and *corecursion* along with many interesting data
structures. In logic it provides *induction* and *coinduction* and the
ability to construct sophisticated theories. In high-school algebra it
produces pure garbage---high-school algebra is without sufficient
structure to "see" fixed points.

[types of data]:http://tel.github.io/2014/07/23/types_of_data/

| Type | Haskell | Logic | High-school Algebra |
|-
| $$1$$ | `Unit` | trivial | 1 |
| $$0$$ | `Void` | impossible | 0 |
| $$\_ + \_$$ | `(+)` | or | addition |
| $$\_ \times \_$$ | `(*)` | and | multiplication |
| $$\_ \rightarrow \_$$ | `(->)` | implication | exponentiation |
| $$\mu \_$$ | `Mu` | induction | $$\infty$$ |
| $$\nu \_$$ | `Nu` | coinduction | $$\infty$$ |

The basic types of data, the first five rows on this table, focus on
the dual concepts of construction and use. They are, however,
fundamentally "small" in the sense that the size of constructions and
uses described cannot be much more complex than the sizes of the types
describing them.[^y-combinator] The last two are different---and this
is hinted by the infinite representation in high-school algebra---in
that they provide principles for repeating patterns: *repeated*
construction and *re*-use. Studying them is to take a very careful
look at infinity.

[^y-combinator]: If you are familiar with the untyped lambda calculus then you might realize that the Y-combinator (or other fixed point combinators of that ilk) allow you to break this size restriction. You'll find however that while the types of data bear a passing resemblence to the untyped lambda calculus they are fundamentally weaker. If the Y-combinator were to be built using the tools presented so far it would have a type like `(A -> A) -> A` for any choice of `A`. Translating that type to an expression of logic should be a hint, but it's an interesting exercise to try to create some form of data with that type.

## Higher-order type schema

Previously I discussed how something like $$\_ + \_$$ is a type schema
of two "holes" which, when filled with concrete types, takes a
particular meaning. A first hint that $$\mu \_$$ and $$\nu \_$$ are
different in that they are type schema which demand that their holes
are instantiated with *other type schema*. In particular, one-hole
type schema.

For instance, the following is a valid construction of a type: $$\mu
(1 + \_)$$.

It's important therefore to spend a little time talking about type
schema. For instance, $$1 + \_$$ can be thought of a way of
transforming one type into another type by adjoining unit.[^maybe] In
general, type schema can be thought of as "patterns" for constructing
types or "contexts" of their use.

[^maybe]: This is known as `Maybe A` or `Optional A` or `A?` but is distinct from the idea of null pointers.

Despite a superficial similarity between type schema and lambda forms,
the notion of a type schema does not necessarily bring with it the
same power of exponentials in data. Doing so---and thus seeing types
as just another layer of data with their own layer of super-types
above for as many layers as you choose---is well-studied but it
obscures some of the meaning of $$\mu \_$$ and $$\nu \_$$ to introduce
it all at once.

## Mu, the infinite bricklayer

The type $$\mu \_$$ is called "mu" or "least fixed-point". It doesn't
exist by default in Haskell. `Mu` is special because 
