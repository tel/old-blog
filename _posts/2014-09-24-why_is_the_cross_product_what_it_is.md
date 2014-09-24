---
layout: post
title: Why the Cross Product Is What It Is
comments_enabled: true
excerpt: When studying sets in mathematics you learn a number of seemingly arbitrary constructions like, for instance, the so-called Cartesian Product. It's natural to ask why these constructions are so privileged... and while the answer is not exactly straightforward, it is very revealing! (An answer to a question posed on Quora.)
---

*I wrote [an answer to a question on Quora](http://qr.ae/ezUAu) which
I wanted to record here as well.*

---

## Q:

**Why is the cross product of two sets all the combinations of their
 elements?**

## A:

When asking something like "why" in mathematics you have to accept
either very short unfulfilling answers or very long involved ones.

The short answer is because that is the definition of the cross
product (on sets) and it's a useful tool that comes up frequently
enough to deserve a name. The long answer hopes to suggest why it is
useful and comes up frequently.

To understand the long answer we have to delve into the fascinating
world of Category Theory. This is a branch of mathematics which
emphasizes studying not objects themselves but the relationships
between objects instead. So, in Set Theory we care about sets. In
Category Theory we care about functions between sets.

In particular, imagine you have in front of you every possible set,
each represented as a dot. All together this is a glorious
constellation of points. We add to it a collection of "arrows", one
for each possible function.

To be clear, this is a lot of arrows. From a set of $$N$$ elements to
a set of $$M$$ elements there are $$M^N$$ arrows. Our constellation
becomes much more like a rats nest of wires. But this complex place is
the domain of Category Theory. What we'd like to do is comb the arrows
and, well as always, find patterns.

One particularly interesting pattern we can detect is that there are
some objects (again, sets) which have this interesting property:

*$$A$$ set, $$S$$, is called "initial" if for any other set $$X$$
there is a unique arrow from $$S$$ to $$X$$.*

These so-called initial sets, in some sense, sit at the "front" of our
wire ball. Based purely upon the kinds of arrows they posses we can
point them out and talk about them. It further turns out that there is
exactly one of these objects and it corresponds to the empty set.

This is cool! And the whole point of Category Theory. We've noticed
that the patterns of the arrows reflects an interesting property of
the objects. Further, we have a really general way of finding these
interesting objects by looking for properties like "initiality". In
general, properties like this are known as universal properties which
suggests their, well, universality.

Here's another warm-up example. Let's look for the opposite
interesting property:

*$$A$$ set, $$X$$, is called "final" if for any other set $$S$$ there
is a unique arrow from $$S$$ to $$X$$.*

This property seems similar to initiality, but, like the name implies,
we're now looking for sets which are at the "back" of the wireball. If
we search around for a while we'll find them: they are all of the
singleton sets—sets with exactly one element. There are an infinite
number of these singletons, of course, but we'll also note that by
finality each pair of singletons, say $$A$$ and $$B$$, have exactly
two functions between them one from $$A$$ to $$B$$ and one from $$B$$
to $$A$$. This ends up being something we call an isomorphism which
can be thought of as a Category Theoretic "contract" that $$A$$ and
$$B$$ must be treated the same.

Since all of the final objects have isomorphisms between them we like
to say that the final objects in set are "unique up to isomorphism"
and we might even like to talk about the final object. Really it just
means what we said above—they're all isomorphic to one another.

Ok, so we've talked about initiality and finality as these universal
properties which allow us to uniquely define the initial and final
objects in our Category Theoretic perspective on sets.

Since my whole goal was to talk about the cross product... I hope
you're jumping to guess that perhaps the cross product can be found
via some other Category Theoretic universal property. And this is
indeed the case.

For any two objects $$A$$ and $$B$$ we consider the set of all objects
$$C$$ such that there are two arrows, call them $$c_A$$ and $$c_B$$,
going from $$C$$ to $$A$$ and $$C$$ to $$B$$ respectively. If you
picture this it forms a little "V" shape with $$C$$ at the vertex and
arrows flowing "outward".

We'd like to look for all such $$C$$s in our rat's nest. There may be
a lot of them but we can see each one as "product-like" because of
those ca and cb functions which extract what we might call the "first"
component and the "second" component. This feels a bit like the cross
product now where we can look at each pair in the cross as having two
sides.

Finally, we'd like to somehow "summarize" all of these $$C$$s using a
universal property. We'll pick this one:

For given objects $$A$$ and $$B$$, an object $$C$$—along with maps
$$c_A$$ and $$c_B$$ from $$C$$ to $$A$$ and from $$C$$ to $$B$$
respectively—is called the "product of $$A$$ and $$B$$" if, given any
other object $$D$$ which also has maps $$d_A$$ and $$d_B$$ there is a
unique arrow from $$D$$ to $$C$$.

In the sense that the final object was the "last" object in the whole
rat's nest we might say that the product of $$A$$ and $$B$$ is the
"last" object in the part of the rat's nest which "looks like pairings
of $$A$$ and $$B$$". If that makes sense.

And finally, if you work out what all of these properties must mean
for sets, it turns out that the Category Theoretic "product of $$A$$
and $$B$$" is just $$A \times B$$. The "why" of its use arises from
the extremely general Category Theoretic mechanism from which it
arises.

As a final note, you might wonder what this generality buys us. It
turns out that Category Theory might be developed on sets and
functions between them like we did here, but it might also be
developed on all kinds of other objects. Off the top of my
head—groups, rings, natural numbers, pre-orders of any kind, boolean
algebras, finite state automata, grammars, "operational paths" in
parlance of Operational Transforms, general relations, vector spaces,
etc etc etc.

In each case the "rat's nest" is called a Category and we can
construct Categories of myriad varieties.

And in each one we can ask, first, "does a product exist?" then "when
does it exist?" and finally "what is it?". In each case we'll find an
object of the category which behaves like a suitable "combination" of
two other objects and will follow an intuition not-so-unlike the cross
product of sets.
