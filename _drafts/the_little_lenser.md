---
layout: post
title: The Little Lenser
comments_enabled: true
---

## Preface

The goal of this post is to show the beauty of working with lenses. I
believe lenses are a natural way to encapsulate the ways which we
break data into pieces either to observe it or change it.

In order to do this, I assume only that you know a little
Javascript. In particular, you should be familiar with the idea that
*functions are values*. From there, I hope to expand your point of
view on what can be achieved from such humble beginnings.

Lenses are a large concept originally, to my understanding, developed
in database theory in order to relate table views to the canonical
tables they result from. Their most sophisticated instantiation in a
programming language is most likely the extensive
[`lens`][^lens-package] package available for Haskell. The small lens
library developed for this post `miniLens` follows many of the idioms
and conventions of Haskell's `lens` so that the knowledge may more
easily transfer. Of course, the concept itself is transferrable.

This post will have a sister post which covers the same material but
uses simple Haskell as the example language. Hopefully it will serve
as a mechanism for connecting the use of lenses in both languages as
well as a mechanism for connecting the two languages themselves.

Finally, this post is inspired by the didactic styles of wonderful
books [*The Little Schemer*][little-schemer] and
[*The Reasoned Schemer*][reasoned-schemer] written by Daniel Friedman,
Matthais Felleisen, William Byrd, and Oleg Kiselyov and illustrated by
Duane Bibby. If you dislike the style of this post, the fault is my
own. The above books are incredible.

[little-schemer]:http://www.amazon.com/gp/product/0262560992/ref=as_li_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=0262560992&linkCode=as2&tag=sdbo07-20&linkId=5H3KGREKNWIAJQDG
[reasoned-schemer]:http://www.amazon.com/gp/product/0262562146/ref=as_li_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=0262562146&linkCode=as2&tag=sdbo07-20&linkId=JJ5CURPKIQVSPMD6

## Introduction

This is a tutorial in which you and I have a conversation about
lenses. Lenses and other "optics" are a way of *focusing* on data and
dividing it into pieces.

Let's dive in.

## A magnifying glass is a wonderful tool

**What is a lens?**

It is a *focusing* on some component of a value.

**What is a value?**

Any concrete thing in Javascript. `3` is a value as is `[1,2,3]` as is
`{number: 3}`.

**What are the components of some value, say `x`?**

Any smaller value "inside" of `x` is a component of `x`.

**Do all values have components?**

Yes, of course.

**What about atomic values like `3`?**

Yes. At the very least the value itself is a component of itself. For
instance, `3` is a component of `3`---it's the whole thing!

**What about function values?**

Well, functions have `.name`s. But those are *metadata* of the
function. So, a function is just another atomic value.

**What is the lens which focuses on the whole subpart?**

It's called `Le.id` for *identity*.

**Why is it called identity?**

Because if you *view* a thing through `Le.id` you just get that thing
exactly.
