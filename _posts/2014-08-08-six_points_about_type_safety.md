---
layout: post
title: Some Points About Type Safety
comments_enabled: true
excerpt: The technical concept "type safety", and even the underlying technical concept "type", is often misunderstood due to being conflated with marketing terms or given undue technical weight. This post enumerates some useful intuition pumps for better understanding, talking about, and consuming material related to type safety.
---

Many programmers are currently interested in the notion of "type
safety" as it is often touted by language promoters to suggest that a
given language is more likely to reject "bad" programs of some notion
or another.

Unfortunately, "type safety" as a term is fairly overloaded and
confusing. Many [words][words] have been spilled trying to clarify it
and I'm about to spill a few more.

The following six points I think cover a few common misconceptions and
terminological differences which will help focus and improve
discussions. *Ultimately* I want to emphasize that the best debates
about types should discuss how different forms of type systems afford
better use through greater richness, how they hold more meaning and
provide [a better UX][ux].

[ux]:https://lobste.rs/s/h9vu5h/what_to_know_before_debating_type_systems/comments/oinwwc#c_oinwwc
[words]:{{ site.url }}/posts/all_you_wanted_to_know_about_types_but_were_afraid_to_ask/

---

### "Type safety" is both a loose marketing term and a *very tight* formal term. The two uses are related but hardly identical.

As a marketing term it tends to refer to the property of a language
where type information reduces the likelihood of writing "unsafe"
programs of some form or another. Typical notions of "unsafety" which
are avoided include: null pointers, memory safety, implicit coercions,
array out-of-bounds mistakes, encapsulation breaking, and more.

As a technical term, which will be the only sense I continue to
consider, *type safety* is a property of a language *in the presence
of* a specification or sense of the language. It ensures that the
*static* semantics of the language are linked to the *dynamic*
semantics.

### Type safety relates a given language to a *choice* of value semantics or analysis.

You cannot reasonably refer to the type safety of, say, C, because you
must also talk about *how you consider or analyze C*[^harper]. It is
always possible to pick an analysis of a language which gives it type
safety---this is equivalent to analyzing a program by running it!
Therefore you probably want to seek out a *useful* analysis where
"useful" may mean some selection of: abstract, high-level, decidable,
easy to understand, built-in.

The last "useful" quality is particularly relevant from a language UX
point of view. Some languages perform automatic type-checking---a mere
form of analysis---and may refuse to compile if that checking fails.
This, in practice, *selects* a particular analysis of the language as
privileged since it's the one that the tools basically ensure nobody
can avoid.

Note that useful is often today a matter of personal choice. Some
small amount of research has been done to consider what kinds of type
systems may afford objective improvements in programming metrics, but
this research is often underpowered, flawed, preliminary. So, instead,
we must often judge usefulness by personal or team values alone and
therefore lose the technical properties of the debate.

### Typing (in the sense of type safety) assumes that you want to analyze the program *without* running it.

Thus it distinguishes between *dynamic*, runtime information and
*static*, compile-time information.

This cleanly separates notions of static types and dynamic types
(sometimes known as "tags" or "classes" to prevent confusion). Since
we're assuming this distinction is important, from now on I'll always
refer to statics when talking about types.

Ultimately, we can understand type safety as saying not much more than
*the dynamics of your language respect the statics of your analysis*.
This helps to emphasize how trivial formal "type safety" is as a
concept---any analysis which did not have type safety ought to simply
be discarded as meaningless. That said, many standards and popular
conceptualizations of languages are *not type safe* yet still popular
because they are useful for other reasons: popularity, simplicity,
convenience.

One good property of a language is its amenability to the introduction
of type safe analyses which are popular, simple, and convenient.

### Types often distinguish certain program utterances or fragments as allowable while others are not.

They perform this task by enumerating the allowable ways to construct
and use [types of data][types-of-data] and introducing a mechanism for
how the *type language* describes each of those operations. In other
words, they create a justification for each well-typed program to
exist and then outlaw the rest.

[types-of-data]:{{ site.url }}/posts/types_of_data/

These justifications concern only the construction of valid language
*syntax*. Then, type safety ensures that runtime dynamics respect
these justifications in two ways: (1) no dynamic "step" will change
the type of a program, i.e. `Int` does not suddenly become `Char`
without justification and (2) any program which has a justified type
has dynamics which are well-defined including runtime error behavior.

These rules are together called "preservation" and "progress".
Notably, they leave a lot of room for the language to define what it
means to justify a change in types (automatic upcasting, for instance,
is interesting) or well-define program dynamics.

An important example of this is that any language admits the *vacuous*
type system by having a single type which justifies all language
syntax and a dynamics where runtime *type mismatch errors* are thrown
whenever operations are considered non-sensible. Oftentimes, dynamic
languages are analyzed using this *unityped* system, though it's
possible that even dynamic languages take advantage of some weak form
of static typing if their compiler does static specialization.

The vacuous type system is simple, but completely useless. Having only
a single type, its type language lacks all expressiveness and thus
provides little insight, analyzability, or use. It's a useful example
however since it explore how very little it can mean to call a
language "type safe".

### Some languages are amenable to expressive static types.

This means that they have a rich static structure which allows for the
elimination of many invalid programs at compile time.

Achieving this often requires picking a particular static analysis as
privileged and thus these languages usually come with an ecosystem of
tools which emphasize the privileged nature of that type system.

Another common inclusion is internalized types where type annotations
and definitions appear as part of the language in order to allow the
programmer to direct how type checking occurs. Often, a good way to
detect when internalized types are static (and thus not tags or
classes) is that they are *erased* at runtime---at least by
default[^typeable]. This means that there is no way for the program's
behavior to change dynamically according to types. Essentially, this
outlaws reflection and, in simple contexts, is usually A Good Thing.

### Type safety need not be absolute in practical languages.

Oftentimes a majority of core primitives are amenable to some brand of
static analysis and that analysis is type safe with respect to them,
but then the language also includes a few "extra features" which have
exotic, potentially dangerous dynamics.

This can be very useful for writing code which is---but cannot be
statically proven to be---type safe. Common examples include shortcuts
taken for efficiency behind abstract interfaces and FFI.

Typically in languages with expressive type safe type systems the
dangers of adding exotic dynamics are high and well-recognized.
Massive effort is taken to ensure that these exotic dynamics are well
protected and end users are encouraged to avoid them at all
costs.[^unsafe]

When these mechanisms are truly unavoidable, they typically mean that
the burden of proving type safety, progress and preservation, has
shifted from the compiler to the programmer. Thus, they are typically
tools reserved for experts.

## Footnotes

[^harper]: Dr. Robert Harper describes such a type safe analysis of C [in a comment here](http://www.pl-enthusiast.net/2014/08/05/type-safety/#comment-500)

[^typeable]: Even if you have a type system which is by default erased it's sometimes possible to retain information from it for purposes of reflection at runtime. Haskell's `Typeable` mechanism does this. Notably, these mechanisms *can* be very dangerous. It's easy to break type safety if they're included. `Typeable` has been fixed in Haskell, but it took some time before its safe semantics were well understood.

[^unsafe]: This is the legacy behind things like Haskell's `unsafeCoerce` which is, as it sounds, highly unsafe. To get access to this (sometimes useful) function you even need to import a special module named, appropriately, `Unsafe.Coerce`.
