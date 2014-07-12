---
layout: post
title: Mutable Algorithms in Immutable Languages, Part 2
---

Last time we saw a way to implement Union/Find, an algorithm which
depends critically on *observable* mutable memory, within a particular
abstract monad called `Mem`. Monads implementing `Mem` model mutable
memory and we saw that's sufficient to recover Union/Find.

But we didn't actually see any implementations of `Mem`. So maybe
we're hosed. Are there any interesting implementations of `Mem` we can
run Union/Find in?

## In case of emergency break ivory

Okay, I'm being dramatic. There is, of course, at least one such
model. If we're willing to *infect* our program with `IO` then we can
always use `IORef`s to model mutable memory. It's almost too simple,
but we'll do it anyway for completeness and to explain some *nits*
involved in working with the `Mem` class.

Essentially, we'd like to have, more or less, the following
correspondences

* `ref` is `newIORef`
* `deref` is `readIORef`
* `set` is `writeIORef`

Hopefully, this also clears up why I called pointers "refs" back in
part 1.

### Diving in


