---
layout: post
title: "All you wanted to know about types but were afraid to ask"
date: "2014-07-08 01:02:06"
---

*Type systems are a hot topic in programming language debates. Often,
 however, the arguments being had spend a lot of time batting about
 the lack of common terminology and shared information. This post
 hopes to reveal a swath of information about types to better form
 common ground for discussion of type systems of all shapes and
 sizes.*

*Corrections and extensions are very welcome.*

---

The word "type" is easy to overload in common usage. In can refer to
things as vague as elements of a domain being considered or as
specific as the formal abstract definition favored by type theorists
today. Furthermore, the presence or absence of types can be seen as a
positive or negative trait of a language and so it's easy to see two
people arguing vehemently about whether or not a language "has types"
even when their definitions of "type" vary.

## A wide array of common uses

In common usage, type might informally mean any mechanism whatsoever
for distinguishing loose or tight classes of things. This is a *very
wide net* to cast and below it's explored a little bit. What's
important to note is not the real breadth of what "type" means but
instead the importance of establishing common ground before debating
about the word.

---

Merriam Webster says

> qualities common to a number of individuals that distinguish them as
> an identifiable class: as (1) : the morphological, physiological, or
> ecological characters by which relationship between organisms may be
> recognized (2) : the form common to all instances of a linguistic
> element

and notes the synonyms "kind", "sort", and "category". In the domain
of programming languages "type" is heavily overloaded between various
classes of "type systems" from dynamic to static, strict to loose.

In order to discuss further it's important to consider the *phase
distinction* in formal languages between their statics and their
dynamics. The *statics* of a language are the properties which are
inferred from the syntax of the language alone---these properties
cannot reference behavior of the language under information available
only at runtime. The *dynamics* of a language are the properties of
how programs in the language evolve at runtime.

Various languages consider "types" to arise as static properties or to
be available at runtime. Sometimes when types are available at runtime
a language is said to have "reflection". Sometimes when types are not
available at runtime a language is said to have "type erasure".

Some types are first class and available for reasoning either within
the language or at least by the user. Some types are exclusively
available to the compiler for accelerating code and are never made
apparent to the programmer. Among those type systems which are first
class you may have a burden laid upon the programmer to annotate their
code with the proper types. You may also have an inference engine atop
that system which eases or obviates the need to annotate programs.

Some types are purely analytic or "pluggable" in that they can
constrain a program or improve our understanding of it but can never
affect the semantics of the program itself. Others are intimately tied
into the semantics of a language such that prior to computing all of
the types of a program it's not possible to know its behavior.

## Formal terminology

Among type theorists there is a more rigorous and streamlined
definition of "type" which captures some (but hardly all) of the
meanings above.

To invent a distinctive word, a "formal type" is a description of the
statics of a language. It's assigned to a fragment of abstract syntax
and it indicates the kind of *value* that the abstract syntax
represents. For example, both `3` and `3+3` considered as abstract,
static syntax might be given the same type, `Natural`, because they
each take values as `Natural` numbers. For the remainder of this
section, the word "formal" may be elided if the meaning is clear.

Formal types give rise to the technical notion of "type safety". A
language and its type system together are *type safe* if the dynamics
of the language respect two properties: *preservation* and *progress*.
Language dynamics *preserve* a type if as a program "evolves" at
runtime its type does not change. For instance, we might consider
evaluation of the abstract syntax `3+3` as stepping like

    3+3
    2+4
    1+5
    0+6
    6

If this evolution does not change the type then the dynamics preserve
the type. A dynamics has *progress* if any abstract syntax either
reflects a *value* of its type or can be reduced. In order to talk
about this we thus have to give a language a notion of base *values*
which every program attempts to compute. For instance, we can talk
about the values of `Natural` as the natural numbers `0, 1, 2, 3,
...`. Thus *progress* entails that any program typed as a `Natural`
either *is* one of those numbers or can continue to evolve.

When a language---its statics, type system, dynamics, and values
together---has preservation and progress then it is type safe. What
this means is that given the type of a program we know something very
clear about the eventual value the program *will* compute.

### The "unityped" argument

One nice thing about the formal types framework is that it can be
applied to languages even if the language itself does not declare a
choice of statics and types.

For instance, we can examine a dynamically typed language in a trivial
sense by giving every syntactically correct program the same type
which reads something like "either and integer or a string or an
exception or a dictionary of integers or a dictionary of strings or an
array of integers or and array of dictionaries of integers or..."
eventually encompassing every kind of value allowable within the
language. Often this large union of types is called `Any`.

Under a type system like this any language is automatically type safe!
This should not be thought of as a strength of the language but
instead it should suggest a crucial weakness to the notion of type
safety itself: if the types are too boring themselves then
preservation and progress don't mean much.

This leads to a desire to give type systems which are refinements of
`Any`. Sometimes compilers of dynamic languages will automatically
attempt to *infer* these more refined types as this can lead to
improved performance. Sometimes first-class type systems allow for
giving both vague and refined types together within a program leading
to *hybrid* type systems where the degree of "interestingness" or
"safety" of the given types can vary throughout a program.

It's also sometimes seen as derogatory when a language is considered
to only have `Any` as its type. It certainly means a lack of formal
type analysis available statically and it suggests that language
features exist which prevent this static analysis from being possible.
However, the lack of a refined type system is different from the
impossibility of finding such a system.

### Pluggable types and general static analysis

Languages which do not fix their formal type system---or, even more
weakly, languages where the semantics are not dependent on the
types---have the option to be analyzed under many different kinds of
statics. These are sometimes called "pluggable types".

In this circumstance the UX of typing may feel very different. Instead
of being something performed at each compilation, static analyses
might be performed optionally by a third-party tool. The analysis may
be partial and multiple tools might be combined together in order to
build a complete static report.

### Languages of formal types

Advanced forms of formal types can involve their own language for
merely describing the types themselves. This often involves the notion
of "type constructors" such as `List` which are applied to other types
like `Integer` to form complex types like `List Integer`, i.e. lists
of integers. Usually these languages are described as "type languages"
while the language being described by the types is the "term
language". So, `[1,2,3]` is a *value* in the *term language* which has
the *formal type* `List Integer` which is written in the *type
language*.

As the complexity of the type system itself grows it may begin to
resemble formal language all on its own and it then becomes desirable
for the "types to have types". This second level of typing is
sometimes called "kinds". The third level is sometimes called "sorts".

It's can be desirable for the type languages at each higher level to
be simpler and simpler so that there isn't an infinite stack of typing
levels to handle. This provides a tradeoff on the expressiveness of
the type language. It's also often desirable for these languages to
not be Turing Complete as it's important to be able to say two types
are equivalent (and thus their programs take the same values). Turing
Completeness would cause equivalence to be undecidable in general and
mean that type checking could sometimes fail to terminate.

On the other hand, some languages have embraced unification of the
types and terms. In other words, the very language which the types
describe is indentical to the language of the types. This produces an
infinite hierarchy of types and all the complexities involved therein.
Typically these languages have "dependent types" which allow
information to bleed *between* the layers of types. It's also typical
that these languages restrict Turing Completeness at every layer for
all the reasons discussed previously.

As a final note, the complexity of type languge is usually related to
the "interestingness" of types. Very boring type languages will have
only very weak notions of "type safety" while very complex type
languages will usually include *both* boring notions of type safety
*and* extremely interesting ones. At the most complex end, dependently
typed languages typically have types which can express *arbitrary
mathematical theorems* and the programs which inhabit these types an
be seen as proofs of validity of the theorem.

More practically, complex type languages can capture information about
complex abstraction patterns being used in the term language.
Typically, a term language with a weak type language will trade off
simplicity and speed of analysis for loss of abstraction power. Since
complex type languages are at the very edge of research today this is
perhaps the *most key* tradeoff in the design of the statics of a
language.

## Dynamic types

Outside of the realm of "formal types" explored previously there are
many kinds of type systems available dynamically in languages. For
instance, many languages express notions of *reflection* which allows
a language to get ahold of the type of a value at runtime. For
instance, in Javascript we can write

    > typeof(3)
    "number"

to reflect on the type of a value like `3`.

The existence of reflection implies that the runtime of a language
maintains fragments of some type language attached to each value. In
formal terms these are sometimes called "tags" to differentiate from
formal types, but in dynamic language communities they are referred to
simply as types.

### Contracts

As dynamic types are available at runtime if type errors occur they
only occur when the program actually runs into an inconsistency of
types. Oftentimes this occurs in dynamic languages as explicit or
implicit *contract checking* where assertions about arguments are made
to ensure that their values are appropriate.

Well-structured contracts can ensure that violations of program
invariants are quickly detected as the program is exercised at
runtime. They also have the full expressiveness of the language
available to them for structuring the details of a contract.

It's often interesting to compare contracts and formal dependent types
as they appear to have similar expressiveness. Contracts operate
dynamically during program operation and check decidable properties of
the values of a running program. Dependent types express static
invariants which all possible runnings of a program will prove.
Complex contracts can be easily added to a program though no matter
how many times you run a program you can never be certain that your
contracts are always upheld. Complex dependent types are very
challenging to introduce to a program as they impose structure across
the entirety of the program eventually ensuring that all possible
runtimes respect the properties of the types.

### Reflection and formal type safety

Languages with reflection are often thought to be formally type unsafe
as you can trivially write programs which reflect on type information
and use it to *coerce*, in other words to change their own type and
break preservation. As discussed before, though, formal type safety
must be considered relative to a choice of formal type system and we
can always type everything as `Any` and get it back.

Ultimately, however, languages must usually trade off between simple
type languages and the power of reflection. Unbounded reflection
usually requires very complex type system to have interesting type
safety. Bounded (a.k.a. "type safe") reflection *is* possible in
moderately powerful formally typed languages, though, and most uses of
reflection can be performed in bounded systems.

## Types as user interfaces

When a language fixes a formal or informal type system it will often
use that type system to provide feedback to the programmer or tester
about errors in the program.

For instance, type information (again, formally, tag information)
available at runtime in dynamically typed languages with reflection is
often used to make meaningful error messages when a program throws an
exception or gets stuck. This improves the programmer user experience
by revealing information available to the runtime system or
interpreter.

The most notorious user experience with types arises in compiled,
statically typed languages where the compiler will refuse to proceed
if it cannot complete a typecheck. In other words, the compiler
examines the syntax of the entire program and ensures that the types
are consistent throughout. When inconsistencies are found it rejects
the entire program and reports the inconsistencies.

Usually, the former user experience is the hallmark of the general
class of "dynamically typed languages" while the latter user
experience is associated with "statically typed languages". In fact,
this user experience distinction is often considered the primary
feature distinguishing the two.

Generally, this is a good heuristic, but it's important to note where
it breaks down.

For instance, some dynamically typed languages with pluggable type
systems might be able to provide ahead-of-time warnings about possible
type failures (see Erlang's *Dialyzer* or Clojure's *typed-clojure*).
It's also very possible to write a compiler for a statically typed
language which compiles programs with type inconsistencies and merely
ignores them until they actually cause problems at runtime (see GHC
Haskell's `-fdefer-type-errors` flag).

Generally it's an open research question as to what the best user
interfaces for information afforded by either formal or informal types
are. This research is well-developed for dynamic types but often more
anemic for static types.

## Types and programming performance

Ultimately, there is usually a desire for empirical evidence that some
kind of type system (or lack thereof) improves coding capacity by some
measure or another. These empirical studies are often valuable, but
also often lacking in the kind of
[internal](http://en.wikipedia.org/wiki/Validity_(statistics)#Internal_validity)
or
[extenal](http://en.wikipedia.org/wiki/Validity_(statistics)#External_validity)
validity needed to draw convincing conclusions.

Lacking potent empirical evidence, performance arguments often come
down to stylistic values or feel which relegates the question again to
one of user experience and psychology.

### Testing versus typing

One argument for style and performance is the difference between
testing and typing. This is common enough to deserve special
consideration.

Program tests are capable of providing evidence for achievement of a
much wider set of goals than the guarantees of types can provide. In
professional programming it's ubiquitous to design program testing
regimes which ensure that deliverables achieve the business value they
were designed to while it's almost inconceivable to believe that even
the very most sophisticated type language could express such a demand.

Type systems are faster to run and provide comprehensive guarantees
(so long as the system exhibits type safety) on program behavior. It
is impossible to write tests which can efficiently enumerate all
possible program states to ensure that no edge cases remain, though
any expressive type system can succinctly eliminate edge cases by
simply making invalid programs inexpressible.

Ultimately, there is very little reason to believe that testing would
subsume all of typing or visa versa, however the middle ground between
the two use cases is sometimes contested. Extensive unit test suites
often test program properties which could be encoded in types and
sufficiently abstract program components can be completely guaranteed
of their value via expressive typing. In this middle ground there is
unlikely to be a silver bullet---specific program performance needs
should be analyzed to determine whether the tradeoffs of static or
dynamic analysis work best.
