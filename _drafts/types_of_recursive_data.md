---
layout: post
title: All Kinds of Freedom
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

In [a previous post][types-of-data] I discussed basically three "Types
of Data" fundamental to logic, computer science, and
mathematics. These types encode many of the basic structures and
actions available to the computer programmer, logician, or high school
student.

[types-of-data]:http://tel.github.io/2014/07/23/types_of_data/

The logician and the programmer should both quickly get bored,
however. The types introduced so far are clearly fundamental but also
*boring*---by which I mean they contain no interesting programs nor
theorems.

In this post, I will expand on those fundamental types by adding two
forms of careful recursion. These forms differ from the types from
before in that they are schema for introducing all kinds of new
types---many of which are very interesting. For instance, by the end
of this post we will have the tools to describe the natural numbers
and some operations on them---from there the integers, rationals, and
so on are available.

There still will remain almost no interesting theorems, but there will
be interesting programs. Importantly, we will have no yet reached a
set of types which describe *Turing complete* programs.

Indeed, much of the point of the constructions in this post is to
provide much of the power of recursion without introducing
undecidability. Equivalently, we'd like to introduce inductive
structure without relying on a function like

~~~
fix :: (a -> a) -> a
~~~
{: .language-haskell}

which is clearly logical gibberish. Introducing it throws away the
ability to express a great deal of interesting theorems. Equivalently,
introducing such a function would throw away the ability to reason
about some programs for the benefit of completeness.

And for the high school student, unfortunately, even the weak types to
be introduced here are too powerful and high school algebra can only
regard them as opaque infinities.

Let me tell you about $$\mu \_$$ and $$\nu \_$$.

---

| Type | Haskell | Logic | High-school Algebra |
|-
| $$1$$ | `Unit` | trivial | 1 |
| $$0$$ | `Void` | impossible | 0 |
| $$\_ + \_$$ | `(+)` | or | addition |
| $$\_ \times \_$$ | `(*)` | and | multiplication |
| $$\_ \rightarrow \_$$ | `(->)` | implication | exponentiation |
| $$\mu \_$$ | `Mu` | induction | $$\infty$$ |
| $$\nu \_$$ | `Nu` | coinduction | $$\infty$$ |



