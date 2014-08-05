---
layout: page
title: Six Points About Type Safety
comments_enabled: true
---

Many programmers are currently interested in the notion of "type
safety" as it is often touted by language promoters to suggest that a
given language is more likely to reject "bad" programs of some notion
or another.

Unfortunately, "type safety" as a term is fairly overloaded and
confusing. Many words[words][words] have been spilled trying to
clarify it and I'm about to spill a few more.

[words]:http://tel.github.io/2014/07/08/all_you_wanted_to_know_about_types_but_were_afraid_to_ask/

1. "Type safety" is both a loose marketing term and a *very tight*
   formal term. The two uses are related but hardly identical.

   As a marketing term it tends to refer to the property of a language
   where type information reduces the likelihood of writing "unsafe"
   programs of some form or another. Typical notions of "unsafety"
   which are avoided include: null pointers, memory safety, implicit
   coercions, array out-of-bounds mistakes, encapsulation breaking,
   and more.

   As a technical term, which will be the only sense I continue to
   consider, *type safety* is a property of a language *and* a
   specification or sense of the language. It ensures that the
   *static* semantics of the language are linked to the *dynamic*
   semantics.

2. Type safety relates a choice of language to a choice of language
   semantics. You cannot reasonably refer to the type safety of, say,
   C, because you must also talk about *how you consider or analyze
   C*. It is always possible to pick an analysis of a language which
   gives it type safety---this is equivalent to analyzing a program by
   running it! Therefore you probably want to seek out a *useful*
   analysis where "useful" may mean some selection of: abstract,
   high-level, decidable, easy to understand, built-in.

   The last "useful" quality is particularly relevant from a language
   UX point of view. Some languages perform automatic typechecking---a
   mere form of analysis---and may refuse to compile if that checking
   fails. This, in practice, *selects* a particular analysis of the
   language as privileged since it's the one that the tools basically
   ensure nobody can avoid.

3. Typing (in the sense of type safety) assumes that you want to
   analyze the program *without* running it and thus distinguishes
   between *dynamic*, runtime information and *static*, compile-time
   information. This cleanly separates notions of static types and
   dynamic types (sometimes known as "tags" or "classes" to prevent
   confusion). Since we're assuming this distinction is important,
   from now on I'll always refer to statics when talking about types.

   Ultimately, we can understand type safety as saying not much more
   than *the dynamics of your language respect the statics of your
   analysis*. This helps to emphasize how trivial formal "type safety"
   is as a concept---any analysis which did not have type safety ought
   to simply be discarded as meaningless. That said, many standards
   and popular conceptualizations of languages are *not type safe* yet
   still popular because they are useful for other reasons:
   popularity, simplicity, convenience.

   One good property of a language is its amenability to the
   introduction of type safe analyses which are popular, simple, and
   convenient.

4. Types often distinguish certain program utterances or fragments as
   allowable while others are not. 

4. All languages automatically admit the *vacuous* type analysis and
   are type-safe under it. This is the type system which merely
   forgets about all typing distinctions by unifying all differences
   in data and execution under a giant "everything"
   [sum type](http://tel.github.io/2014/07/23/types_of_data/) type.

   As an analysis we can consider the usefulness of this vacuous type
   system. It is good for being type safe and simple, but bad in that
   it tells nothing useful about the language statically and
   eliminates invalid fragments whatsoever. As an analysis, its
   purpose is mostly didactic---it emphasizes that all languages can
   be statically typed and that the mere notion of "having static
   types" is essentially useless.

5. Some languages are amenable to expressive static types. This means
   that they have a rich static structure which allows for the
   elimination of many invalid programs at compile time. Achieving
   this often requires picking a particular static analysis as
   privileged and thus these languages usually come with an ecosystem
   of tools which emphasize the privileged nature of that type system.

   Another common inclusion is internalized types where type
   annotations and definitions appear as part of the language in order
   to allow the programmer to direct how type checking occurs. Often,
   a good way to detect when internalized types are static (and thus
   not tags or classes) is that they are *erased* at runtime---at
   least by default[^typeable]. This means that there is no way for
   the program's behavior to change dynamically according to types.
   Essentially, this outlaws reflection.

6. Type safety need not be absolute in practical languages. Oftentimes
   a majority of core primitives are amenable to some brand of static
   analysis and that analysis is type safe with repect to them, but
   then the language also includes a few "extra features" which have
   exotic, potentially dangerous dynamics.

   This can be very useful for writing code which is---but cannot be
   statically proven to be---type safe. Common examples include
   shortcuts taken for efficiency behind abstract interfaces and FFI.

   Typically in languages with expressive type safe type systems the
   dangers of adding exotic dynamics are high and well-recognized.
   Massive effort is taken to ensure that these exotic dynamics are
   well protected and end users are encouraged to avoid them at all
   costs.[^unsafe]

[^typeable]: Even if you have a type system which is by default erased it's sometimes possible to retain information from it for purposes of reflection at runtime. Haskell's `Typeable` mechanism does this. Notably, these mechanisms can be very dangerous. It's easy to break type safety if they're included.

[^unsafe]: This is the legacy behind things like Haskell's `unsafeCoerce` which is, as it sounds, highly unsafe. To get access to this (sometimes useful) function you even need to import a special module nammed, apropriately, `Unsafe.Coerce`.
