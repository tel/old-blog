---
layout: page
title: If you write a monad tutorial...
---

> A tutorial to monad tutorials.

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

All of the methods of "getting monads" up to this point were highly
practical. These kinds of desires stem from wanting to exploit the
power of new techniques often described as monadic.

But there's another side to monads as well—they form powerful analytic
tools. It's not uncommon to see them used in this fashion. Common
examples include the "programmable semicolons" notion, comparative
power analysis between monadic and applicative code, or recognition
that certain algorithms are "essentially monads" of various flavors
such as parser combinators.

There's a practical edge to this as well. Learning to really
understand the monad pattern—especially how it can be extended and
decomposed—can make you a more efficient, confident coder. To return
to the parser combinators example, there's something really efficient
about being able to say that `Parser`s producing values of type `a`
are "just" `StateT String List a` (what's the problem?)

### Semantic islands

This effect of monads comes from what I like to call their nature as
"semantic islands". This is a possible translation of a statement like
"reading from and writing to files lives in the `IO` monad"—you might
say, "reading from and writing to files is constrained to live in
semantic islands of kind `IO`".

What do I mean by semantic island? Monads, especially when used in a
type strict language like Haskell or Scala, are infectuous
computations. Once you've begun to use values "inside" a monad you
start to spend more time "lifting" non-monadic computations up into
your monad than you do taking things out.

To use `Maybe` as an example, once you've begun to work with code that
might fail any new operations performed on results from that code will
be "infected" by the notion of failure as well. For instance, if you
have a `Maybe Integer` then when you lift an operation like `timesTwo`
to operate on that `Maybe` value you receive another `Maybe` value as
a result. Your new computation was infected by the fact that it may
potentially fail.

The only way to "escape" the `Maybe` monad is to explicitly provide a
way of handling the failure circumstance. Most likely, this would be a
default value and so we have functions like

~~~ haskell
def :: a -> Maybe a -> a
def x maybeA = case maybeA of
  Nothing -> x
  Just a  -> a
~~~

which act as rescue planes landing on our desert `Maybe` island,
explicitly taking people back into the larger world.

This might at this point feel like a stretched metaphor, but it plays
out in code written in a heavily monadic style. You end up
constructing specific islands to your task and writing algorithms
which are natural in that place: on `Maybe` island, tasks may fail, on
`List` island, tasks are non-deterministic. Then, we explicitly build
bridges between these islands such as

~~~ haskell
firstResult :: List a -> Maybe a
~~~

which takes a non-determistic task from `List` island to a task which
merely may fail living on `Maybe` island.

As a final stretch of this metaphor, it might be worth considering the
Haskell notion of the `IO` island. This is where the runtime, where
the "real world" can be approached and thus it's required that your
program eventually builds a bridge to there. This can be seen as just
an artifact of the runtime: Haskell executable programs are expected
to have an entry point that looks like

~~~ haskell
-- Get to IO-island with no particular value whatsoever
main :: IO ()
~~~

and the runtime system does nothing more than fly the rescue plane
from `IO`-island back to the real world.

### Nobody cares about "semantic islands"

Many monad tutorials target the metaphor I just laid down above, these
"islands". As a result, many people feel like "getting" that metaphor
is the same as "getting monads".

That's a plausible goal to have, but a strange one. The metaphor I
outlined above is just a (subpar) description of what monads feel
like. Understanding it will help you to have a conversation with me
and maybe even to understand some of the conversations of others
involving monads, but it's hardly a pre-requisite.

The same thing goes for monad metaphors like spacesuits and burritos
and factory floors covered in conveyor belts.

The fact of metaphors like this is that if you already understand the
concept then they are colorful ways of talking about it. If you don't
already understand the concept then hearing an artistic depiction of
part of it won't help you much. This is the genesis of the so-called
"Crockford's Law"

> Once you get monads, you're no longer capable of explaining them.

More directly, the problem is that being able to understand and
generate metaphors like these is just one not-terrifically-useful mode
of "getting monads", but it's still somehow pleasurable in the same
way that reading a novel about love is. If you love someone then a
great artist can touch you with their words, but reading is hardly a
suitable substitute.

### Monads are never the key

Now I'm going to claim that the idea of "semantic islands" is an
interesting one worth studying. The reason is not "monads" at all, but
instead that "getting" semantic islands will sharpen your sensitivity
to the behavior of formal languages—any and all programming languages
included.

In particular, we'd like to become sensitive to the notion of what
even the simplest, most ignorable components of a language's syntax
might *really mean*. Through this process, we'd like to challenge our
mental models of the specification of programs. For instance, let me
introduce the simple language Sum. It consists of a number of
statements, each of which a number. Statements may be separated by
linebreaks or semicolons. The meaning of a Sum program is the sum of
the numbers represented in each statement. The program

~~~
1;
2;
3;
~~~

"means" `6`. If you spend enough time thinking about what the nature
of semantics of formal languages is then you might uncover the answers
to silly sounding questions like "what does 'means' mean?". There's
vast richness here and if you're, say, a DSL engineer then you may
benefit a lot from studying it.

It also so happens that imperative languages with a certain type of
variable binding follow the monad pattern. In fact, you might say that
the monad pattern is the simplest, most obvious way of having an
imperative language with variable binding. It thus forms a very good
starting point to build your own langauge or analyze someone else's.

### Using semantic islands as a tool

The final reason why semantic islands might be interesting is that
you're using a language which has a lot of them. In particular, it's
likely a language which "has monads" in either the weak or strong
sense outlined above and so you see the artifacts of these semantic
islands when reading other people's code.

This is a double-edged sword for these languages, especially in
languages which have monads in the strong sense of strong typing and
higher-order abstraction. In this case, you will see various semantic
islands appearing with almost all distinguishing marks elided and
under the careful watch of a compiler which assumes that those
distinctions are just obvious to you.

When that's true it's a wonderful place to work. When it's false, you
may be in for a nightmare. In both cases, the effect is only partly a
matter of monads and mostly a matter of the powerful tools of
abstraction being employed. If you understand *those mechanisms* well
and you understand the underlying type then you will be able to slowly
tackle that code without "getting monads" at all.

Ultimately, understanding the monad pattern is a shortcut to using the
higher-order abstraction techniques of your language. It can also be a
shortcut to implementing new imperative-language-with-variable-binding
DSLs if that's what your goal is. In both cases, the shortcut is best
employed once you know well the long route.

And, truly, the "power of monads" in this scenario comes from a deep
understanding of the power of the abstraction methods which allow the
language to "have monads" in the first place. For instance, in Haskell
things like `Functor` and `Applicative`, `Monoid` and `Arrow`,
`Semiring` and `VectorSpace` all exist at roughly the same level as
`Monad`. Understanding any of these other patterns may be more
valuable to you than `Monad`.

## Categorically getting monads

There's at least one more, final notion of "getting monads", their
existence as mathematical objects neatly embedded in the entire field
of Category Theory.

This is perhaps the least useful point-of-view as a programming
tutorial, although many people who "really get monads" today also "get
category theory" so it's tempting to study it. Further, people who get
category theory may suggest that CT can be useful for expanding your
sensitivity to patterns in programming.

In my perspective, this is true. In a similar way, learning to fly a
plane will increase your sensitivity to patterns in flying on a
commercial airliner. You might even eventually benefit from being able
to fly your own plane from place to place completely subverting the
need for commercial airliner and opening up the opportunity to land at
a small, rural airport much closer to your ultimate destination.

Many people fly every day without this sensitivity, however.
Similarly, many programmers will never learn CT to basically no ill
effect.

On the other hand, if you're already comfortable reading and grasping
abstract mathematics then learning category theory might be a
relatively fast way of tackling many of the other methods of "getting
monads". It will at least supplement your understanding.

Finally, this mode of "getting monads" enjoys two great benefits: it's
wonderfully terse and highly accurate. No other method of "getting
monads" comes even remotely close. If you "get monads" in many other
senses of the phrase and can spend whatever it may take in time and
sweat and blood to learn the background to understanding the CT
definition then it will focus and compress your understanding of the
pattern to a sharp point.

Here are a few expressions of it:

* If $$F \vdash G$$ are adjoint functors, then their closure, $$G
  \circ F$$, is a monad.
* A monad is a monoid in the category of endofunctors
* A monad is a triple $$(T, \eta, \mu)$$ where $$T$$ is an
  endofunctor, $$\eta$$ a natural transformation $$1 \to T$$ (where
  $$1$$ is the identity functor), and $$\mu$$ a natural transformation
  $$T \circ T \to T$$ such that
  * $$\mu \circ T\mu = \mu \circ \mu T$$ as natural transformations
    $$T \circ T \circ T -> T$$.
  * $$\mu \circ T\eta = \mu \circ \eta T = 1$$ as natural
    transformations $$T -> T$$.

These are not meant to illuminate for anyone who isn't both already
familiar with every word in these sentences and willing to study the
short definition itself for possibly quite long period of time. Worse,
every word in those sentences is a topic whose study is more than
large enough to take longer than most senses of "getting monads"
themselves.

On the flip side, these wonderfully general definitions are powerful.
For instance, we can trivially dualize the first two definitions in
order to produce a new concept called a "comonad":

* If $$F \vdash G$$ are adjoint functors then $$F \circ G$$ is a
  comonad
* A comonad is a comonoid in the category of endofunctors.

We can also examine the third to build deep understandings of what it
means to "follow the monad laws". We can leverage the concepts used in
the definitions to analyze the monad pattern deeply. For instance, if
we know about the notion of "free monoids", and indeed perhaps have
already implemented them as standard `List` types, then we can build a
valid notion of a "free monad"

* The free monad is the free monoid in the category of endofunctors.
