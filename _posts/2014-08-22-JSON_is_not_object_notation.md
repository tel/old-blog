---
layout: post
title: JSON is not Object Notation
comments_enabled: true
---

This is a silly nit to pick, but here goes.

---

*Hi, future reader! If you are at this point rolling your eyes
 because, yes, of course, (a) notation and objects are different or
 (b) it's all just a name anyway---sorry about that.*

*This post is secretly about the expression problem, or, better, the
 differences between initial and final data, between data and codata.
 It was just inspired by a name.*

*If that latter topic sounds interesting, I encourage you to keep
 reading. I get there soon enough.*

---

JSON, the universally known and loved interchange medium of the 21st
century, stands for JavaScript Object Notation. This is due to the
fact that JavaScript Objects have a literal syntax which mostly
resembles JSON.

~~~
[ { "foo": "bar" }, { "bar": "foo" } ]
~~~
{: .language-json}

This is all fine and good.

---

[LCON](https://github.com/ar-nelson/lcon) is a new project I'm going
to pick on[^to-be-clear]. It's roughly the same idea as JSON but with
a different concrete syntax that's supposed to be more human
writable. It stands for "Ludicrously Compact Object Notation".

Except this *really* isn't an "object notation". And that's a *good*
thing.

[^to-be-clear]: To be totally clear: LCON maintainers, should you read this, keep the name. Actually changing it for such technical reasons is sillier than this post.

---

The notion of "object" is hard to define---I'm going to pick a few of
my favorite concepts and presume that this all makes sense for your
own choice---but by nearly any definition *JSON* does not represent
objects.

For instance: let's say, an object is a thing with

1. Extreme late binding
2. Encapsulation of state
3. Messaging (coalgebraic interface)

Javascript objects probably achieve (1) and (3). Maybe (2) if you wave
your arms a bit. JSON achieves *absolutely none of them*.

And again, that's a good thing

---

Javascript doesn't really distinguish too much between things that are
objects and things that are not. It takes the standard "turtles all
the way down---sort of" approach which extends late binding and
messaging to all values in the language.

So one of the best ways to separate out the parts which don't require
late binding and messaging is to serialize the objects reducing them
purely to their internal state. Typically this mechanism of
serialization is JSON.

Which brings me to my point. While JSON is a notation for JSOs, it's
almost the exact opposite of an Object Notation for Javascript. It has
no notion of binding, messaging, functions, encapsulation,
inheritance, delegation, protection, polymorphism, recursion, classes,
abstraction, instances, methods, &c.

That is great!

---

Facebook's [React](http://facebook.github.io/react/) does something
really interesting---it forces you to think hard about the minimal
representation and locality for your *state* in your application. It
then pretty much asks you to represent it as something warmly referred
to as a "Plain Old _ Object" in languages like Ruby or Java.

What is a PO_O? One way to see it is as an object which can be cleanly
serialized to and deseraialized from JSON. It's an object which has
been stripped of binding, messaging, functions (*probably*),
encapsulation, inheritance, delegation, protection, polymorphism,
recursion, classes, abstraction (*probably*), instances, methods, &c.

What does React do this? Because it realizes that creating a partition
between your state and your objects helps to make abstraction and
encapsulation easier. Spreading that state too thinly across your UI
leads to update anomaly bugs and lesser ability to optimize UI code
(e.g. via their virtual DOM).

Having really plain state and operating on it is a really useful
thing.

---

One way I like to look at all this is data versus what's called
"codata". PO_Os are almost certainly data in the sense that their
representation is very, very close to their mechanism of construction.
Their use is finite. Their presentation serializable. Their behavior
inert.

They're about the most boring thing imaginable. All interesting parts
tend to be shifted outside of *data*.

Codata is the opposite. It's a thing in flux, defined by how you use
it. Objects (of various flavors) are great examples of codata as they
endeavor to get you to send them messages and handle their reactions
rather than probe them for their simple, inert, finite presentation.

Codata is fundamentally unserializable. It's just too dynamic.

So what happens when you want to serialize some process that's carried
out in your objects, in your codata? Well, you recognize some way of
"freezing" your object's internal state and then you pop out some
JSON.

---

So that's my whole point. JSON isn't "object notation" because it
doesn't have anything to do with objects. It's finite, inert, boring
in a way which enables its serializability and makes it miraculously
useful.

You like JSON because it's *not* an object.

Which is something to think about, I think, if you're the kind of
person who spends a lot of time in OO languages. OO has a lot of
benefits, but it tends to make you more blind to simple "data" values.
Everything ought to have methods, encapsulated internal state,
inheritance, and classes, right?

Obviously, I'm being facetious. But what does a real marriage of these
two concepts look and feel like? Such a mode of use would marry the
advantages of each side of the
[expression problem](http://en.wikipedia.org/wiki/Expression_problem)
and temper their disadvantages.

---

And in the mean time, don't steal the "-ON" part of JSON and attach it
wherever. You probably aren't making an object
notation[^so-what-is-an-object-notation] and you probably aren't
making a "JavaScript Object" notation.

[^so-what-is-an-object-notation]: After batting around the bush this long, *what is an object notation*? There are many, but a particularly relevant one for this post is, well, *Javascript*. This probably clarifies what makes JSON and other not-object-notations so great---you don't have to `eval` them.

## Footnotes
