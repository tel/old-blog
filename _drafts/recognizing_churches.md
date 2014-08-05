---
layout: page
title: Recognizing Churches
---

After learning about the Church encoding[^well, not church] of
datatypes in Haskell it might be unclear why you should care---after
all, one of the first things you do is show that it's identical to
using constructors. However, the Church/BB encoding reflects a
viewpoint of the *use* of a type instead of the *construction* of it
which can be useful.

The [`logict`](http://hackage.haskell.org/package/logict/) package is
quite interesting to explore. If you've been learning Haskell for long
enough then you've probably heard that the list monad models
non-determinism. If you keep listening you probably have also heard
that (a) it's a bit too *depth-first* and (2) that it's less than
obvious how to make it a monad transformer like we might want.

[`logict` solves both of those problems](http://www.cs.rutgers.edu/~ccshan/logicprog/LogicT-icfp2005.pdf)
and more, and it's relatively easy

[^well, not church]: Well, not the real *Church* encoding, but instead either the *Boehm-Berarducci* encoding.
