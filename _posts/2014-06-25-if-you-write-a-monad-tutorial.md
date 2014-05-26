---
layout: page
title: If you write a monad tutorial...
---

*DRAFT*

Monads tutorials are a pack of lies. They form a tiny nucleus within
the body of work "about monads" but enjoy a disproportionately large
audience among people "interested in monads". This occurs because
monads are spooky sounding yet are often spoken of as if it were a
badge of honor to "get monads". The latter generates a lot of interest
and the former suggests the need for plain English material.

So in combination we get market for tutorials which promise to get you
to *getting* monads cheaply. Which is tough, because it's not even
particularly clear what "getting monads" gets you.

## Getting monads

"Monad" describes an abstract pattern in category theory, type theory,
and formal languages like programming languages. To many, "getting"
monads is sufficiently covered by simply understanding how they
operate within the regime of one or two programming languages.

### The Haskell beginner

For instance, the basest form of "getting monads" available to a
Haskell novice involves constructing `do` syntax. This is a
persistent, thorny desire since Haskell uses monads for "simple"
operations like printing and reading text.

~~~
main = do
  name <- getLine
  putStrLn ("Hello, " ++ name)
~~~

As syntax, there's nothing scary about this greeter program, but as
syntax it's (a) a brittle understanding which shatters when flexed and
(b) prone to challenge from both introductory Haskell materials and
messages from the compiler. The `do`-syntax mechanics involve some
functions which happen to follow the monad pattern and so it is
compelling to try to "get monads" shortly after being burned doing
Haskell IO.

Unfortunately, that's a terrible choice. More important than "getting
monads" is getting

* some amount of how functional programming works,
* some amount of how Haskell's type system works,
* some amount of how Haskell's type-class overloading system works, and
* some amount of the (unusual) Haskell standard library

Understanding these is far more basic to that greeter program than
"getting monads". But if you search for `do`-notation in the Haskell
documentation you'll certainly find monads first.

### The "programmable semicolons" victim

Worse off than the Haskell novice is the Javascript expert who has
heard that monads form "programmable semicolons" and Javascript "has
monads" because it "has semicolons" and "is functional".

Again, it's worth emphasizing that "monad" describes a pattern. This
description has three nice properties

* it's very minimal
* it's very simple
* it's very analyzable

and these properties entail that many things can be described as
monads and a deep understanding of them can arise from this
comparison. For instance, the pattern of comparing paths in a DAG to
the edges they are composed of is a monad and this insight can teach
you a lot about graphs.

More to the point, "semicolons" often can be seen as a monad. This
wonderful observation relates the class of imperative languages to a
large body of comparative work. It's a guilty pleasure to "get monads"
at this level.

Unfortunately, all of this has almost nothing to do with Javascript
"having monads". Especially if someone wants to claim that Javascript
"has monads" in the same way that Haskell "has" them.

In any language with first class closures you can embed choices of
`return` and `bind` functions (a.k.a. `eta` and `mu` natural
transformations) if you follow a certain "typing" discipline. This
allows you to "have monads" in a weak sense.

If your language also has "good" polymorphism, then you may be able to
abstract over these selections of `return` and `bind` functions and
represent the monad pattern generically. If your language has "good"
types then it will be possible to conspire with the compiler to reason
statically about this abstract pattern. Together, these form the
notion of "having monads" in a strong sense.

But everything I've said in the last two paragraphs is identically
true of the pattern of "monoids" and "groups" and "vector spaces" and
"objects" and "singletons" and so on and so forth. The real complexity
and power being discussed is the interaction between higher-order
abstraction capabilities and strong types.

So it becomes murky again what "getting monads" really gets you in
this circumstance.

## Getting a monad or two

More fruitful than "getting monads" in either of the examples given
above is the idea of "getting a particular monad or two". Inevitably,
this means having a conversation about `Maybe` (or `Option`).

### Maybe you get maybe

Many languages have the notion of nullity. No one versed in formal
languages likes this fact and the inventor of nullity (indeed, someone
had to invent it) did so for apparently no other reason than because
it was easy.

> My goal was to ensure that all use of references should be
> absolutely safe, with checking performed automatically by the
> compiler. But I couldn't resist the temptation to put in a null
> reference, simply because it was so easy to implement. This has led
> to innumerable errors, vulnerabilities, and system crashes, which
> have probably caused a billion dollars of pain and damage in the
> last forty years.
>
> — Tony Hoare

There are roughly two problems with nulls. First, typically it's the
case that they can arise wherever, thus being the inevitable wrench in
the works capable of showing up anywhere. Second, nulls are either
contagious (like IEEE 754's `NaN` values or Perl's concept of "taint")
or explosive (like C or Java's `NULL` reference) in that when they
show up where they don't belong they either cause silent failure or
carnage with little control.

Together these problems mean that nullity erodes trust in your system.
A set of secret, destructive edge cases lurking within every component
and mandating either comprehensive, wall-papered nullity checks or a
robustness to catastrophe in order to survive.

In effect, nullity tends to be bad because it's not explicit,
delimited, or semantically clean. Strong type discipline can solve
both of these issues with `Maybe` types.

`Maybe` is a marker on the type of a value which indicates that the
value is "maybe" null. Operations which interact with `Maybe`-values
must explicitly denote that they accept and/or return `Maybe` values.
These typing constraints can be enforced by your compiler (which is
convenient) or by hand (which is more widely applicable) and if done
so comprehensively then `Maybe` solves the two problems from before.

First, `Maybe`-types explicitly denote values which may be null.
Second, operations must explicitly denote that they are `Maybe`-tinted
operations and thus provide a handle for picking the appropriate
error-handling semantics at every step.

The only downside is that this leads to a duplication of every type
and every operation. Powerful language tools should be employed to
mitigate this duplication of entities.

This is where many language communities stand today—accepting of the
value of (at least partially) explicit tagging of failure via a
technique like `Maybe` and needful of mechanisms to handle the
duplication of entities `Maybe` demands. Fortunately, languages with
first-class closures have a few very powerful tools for doing this.

The first is "lift" which transforms a `Maybe`-ignorant function to an
explicitly "short-circuiting" one. It has a Haskell type of

~~~
(a -> b) -> Maybe a -> Maybe b
~~~

and in Javascript, where missing values are still denoted by
`undefined`, it looks like

~~~
function lift(f) {
    return function lifted_function (x) {
        if (typeof x == 'undefined') {
            return undefined;
        } else {
            return f(x);
        }
    };
}
~~~

The second is "default" which transforms a `Maybe`-value into a
trusted, "definitely there"-value. Again, the Haskell type would be

~~~
a -> Maybe a -> a
~~~

while the Javascript implementation (in "curried" form) is

~~~
function def(x) {
    function defaulting_function(maybe_x) {
        if (typeof maybe_x == 'undefined') {
            return x;
        } else {
            return maybe_x;
        }
    }
}
~~~

The third is called "then" and is used to sequence `Maybe`-tinted
operations. It takes a `Maybe`-value and adjoins a continuation which
may itself result in a `Maybe` value again. Its Haskell type is

~~~
Maybe a -> (a -> Maybe b) -> Maybe b
~~~

and its curried Javascript implementation is

~~~
function then(maybe_x) {
    function (continue) {
        if (typeof maybe_x == 'undefined') {
            return undefined;
        } else {
            return continue(maybe_x);
        }
    }
}
~~~

If you have these functions, two of which are higher-order functions,
then you have a powerful toolkit for dealing with `Maybe`-values quite
explicitly. For instance, we could define division to fault to nullity
on dividing by zero (abandoning the nullity system of IEEE 754 for one
hosted entirely in Javascript)

~~~
function div(a, b) {
    if (b == 0) {
        return undefined;
    } else {
        return (a/b);
    }
}
~~~

and then use our toolkit to explicitly siphon along nullified values,
introducing short-circuiting and defaulting as desired.

~~~
var x, an_x;

// This style is more for being illustrative than being idiomatic
x = div(1, 0);
x = then(x, function (definite_x) { div(definite_x, 10) });
x = lift(function(definite_x) { return (definite_x + 100) };
an_x = def(0)(x);

console.log(an_x); // will not encounter `undefined`!
~~~

Further refinements might make the values of missing values more
apparent than `undefined` or sneak these functions more deeply into
the syntax of the language to make it feel natural and idiomatic. Done
properly these are tools that will allow you to consistently rid
yourself of the downsides of nullity.

#### Maybe a monad too

It just so happens that `Maybe` follows the monad pattern. Knowing
this can be powerful. It, for instance, gives you the ability to mix
the "failing nature" of `Maybe` into other monads in a sensible,
consistent way. If your language "has monads" in the strong sense
suggested earlier (and Javascript does not) then you can use those
facilities to abstract over the functions in your `Maybe` toolkit,
highlighting their generality.

But that's, to a first approximation, all you get for "getting the
`Maybe` monad". Its value is mostly contained in its *own, individual*
meaning more than the general meaning the monad pattern highlights.

Many language communities would do well to spend time "getting Maybe"
more than "getting monads".

### Other individual monads

In the same way that `Maybe` embodies explicit handling of failure,
other types can embody explicit linguistic "shades". Lists equipped
with an operation called "concat-map" embody depth-first search.
Equipped with an operation called "interleave-map" the embody
breadth-first search. State-transformer functions embody backtracking.
Lazy streams embody unexhaustable sources. Continuation-passing style
embodies non-linear flow control.

Each of these types have rich semantics all of their own. Some of them
have operations which embody the monad pattern.

## Getting to semantics

- "the programmable semicolons stickler"
- "monads as semantics islands"
- "a glimpse of 'why do we care'"
- "the barrier of starting to see PL as formal languages"
- "the monad laws, tightness, and the-most-simple-thing"

## Categorically getting monads

- "the most abstract sense"
- "the *meaning* of category theory"
- "comparisons in mathematics"
- "category theory for scientists"
- "monads made difficult"
- "just a monoid in the category of endofunctors"
- "generalizations"
  - "comonads, co- everything"
  - "the typeclassopedia"
