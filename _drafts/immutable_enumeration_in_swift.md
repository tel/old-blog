---
layout: post
title: Immutable Enumeration in Swift
comments_enabled: true
---

I'd like to study a very particularly nice Swift protocol: the *view
from the left* which looks a bit like this

~~~
protocol Viewl {
    typealias El
    func uncons() -> (El, Self)?
}
~~~
{: .language-swift}

In plain English, a type which instantiates the view from the left is
a container of elements of type `El`[^el-type] which allows us to
"`uncons`truct" elements from the left. Here, `uncons` means that we
take one element away from the contain and return that element and the
remainder. This is marked optional in the case that we have an *empty*
container.

[^el-type]: It's a little annoying that we have to provide this typealias, `El`, to access the element type of the container. It wouldn't be so bad except that I'm already duplicating the `Element` type which appears in a few of the Swift standard library containers because it isn't bound to a protocol and so I can't depend upon it when defining `Viewl`.

The most obvious type conforming to `Viewl` is the immutable linked
list. We'd like to implement it as follows

~~~
enum List<A>:Viewl {
    case Empty
    case Cons(A, List<A>)

    typealias El = A
    func uncons() -> (A, List<A>)? {
        switch self {
        case Empty: return nil
        case Cons(let x, let xs): return (x, xs)
        }
    }
}
~~~
{: .language-swift}

but due to a bug in the Swift compiler we need to provide a little
indirection through a `class`.

~~~
class Ptr<T> { let val: T; init(_ aVal:T) { val = aVal } }

enum List<A>:Viewl {
    case Empty
    case Cons(A, Ptr<List<A>>)

    typealias El = A
    func uncons() -> (A, List<A>)? {
        switch self {
        case Empty: return nil
        case Cons(let x, let xs): return (x, xs.val)
        }
    }
}
~~~
{: .language-swift}

It's a little warty, but works (and even compiles, unlike some of my
previous posts). We now can view the first element of a finite `List`
using `uncons`.

~~~
(swift) List.Cons(3, Ptr(List.Empty)).uncons()?.0
// r0 : Int? = Optional(3)
~~~

The rest of this article elaborates on the `Viewl` protocol and some
of its uses.

## Standard left-viewable types

One nice thing about Swift is that it leaves types open for extension.
We can thus extend types like `Array` and `String` to conform to
`Viewl`

~~~
extension Array:Viewl {
    typealias El = Element
    func uncons() -> (Array.Element, Array)? {
        return isEmpty ? nil : (self[0], Array(dropFirst(self)))
    }
}

extension String:Viewl {
    typealias El = Character
    func uncons() -> (Character, String)? {
        return isEmpty ? nil : (self[startIndex], dropFirst(self))
    }
}
~~~
{: .language-swift}

These both have really nice implementations of `uncons` because they
conform to `Sliceable`, too.

~~~
(swift) "foo".uncons()?.0
// r0 : Character? = Optional(f)
(swift) "".uncons()?.0
// r1 : Character? = nil
(swift) [1,2,3].uncons()?.1
// r2 : [Int]? = Optiona([2, 3])
~~~

So far these have all been obvious implementors of `Viewl`, but there
are fairly unobvious ones as well. For instance, "increment" is a left
view of a kind

~~~
// I don't actually recommend this, but you *can* do it

extension Int:Viewl {
    typealias El = Int
    func uncons() -> (Int, Int)? {
        return (self, self+1)
    }
}
~~~

Now that we have a few example implementors, we can start to really
examine the power of this protocol.

## Generic functions on `Viewl`

What can we do if we have a type which we know conforms to `Viewl`? So
far, we've identified the ability to look at the `head` and `tail` of
it[^carcdr]

[^carcdr]: Or `car` and `cdr` if you are so inclined. I am not.

~~~
func head<X:Viewl>(x: X) -> X.El? { return x.uncons()?.0 }
func tail<X:Viewl>(x: X) -> X?    { return x.uncons()?.1 }
~~~
{: .language-swift}

We also can "read off" some number of elements of any `Viewl`-able
type into an `Array` and return it and the remainders

~~~
func leftArray<S:Viewl>(n: Int)(s: S) -> ([S.El], S) {
    var res:[S.El] = []
    var s_  = s
    for i in 0..<n {
        if let (a, ss) = s_.uncons() {
            res += a
            s_  = ss
        } else { break }
    }
    return (res, s_)
}
~~~
{: .language-swift}

### Encodings and foldings

If you spend a lot of time playing with `Viewl` you'll probably notice
the following pattern showing up a lot

~~~
if let (x, xs) = input.uncons() {
    return ...
} else {
    return ...
}
~~~
{: .langauge-swift}

Seeing as Swift is a functional language, we should see if we can
abstract that pattern. Indeed we can, in two ways.

The first is perhaps the most obvious way we could abstract this
encoding, although it has a slightly unobvious type and a highly
unobvious common name

~~~
func scott<S:Viewl, R>(zero: R, combine: (S.El, S) -> R)(subject: S) -> R {
    if let (x, xs) = subject.uncons() {
        return combine(x, xs)
    } else {
        return zero
    }
}
~~~
{: .langauge-swift} 

Again, ignoring the type and name, this function is nothing more than
the exact template I wrote above but replacing the `...` bits with
functions and constants reflective of the values in scope at that
point. I called it `scott` because this is somewhat well-known as the
"Scott encoding of a recursive data type" which compares with the
["Church encoding of a recursive data type"](http://en.wikipedia.org/wiki/Church_encoding)
which is rather well-known.[^Boehm-Berarducci]

[^Boehm-Berarducci]: Or, really, since we're in a typed language we don't want to talk about Church encodings but instead [Boehm-Berarducci encodings](http://okmij.org/ftp/tagless-final/course/Boehm-Berarducci.html)... which are almost universally called "Church encodings" anyway since if you erase the types the two are identical.

In fact, this Church encoding can be given as well. We do the same
thing as the Scott encoding, but instead of returning the "remaining"
part of our `uncons`tructed type, we recursively encode it as well.

This is probably more well known as `reduce` or `fold`[^fold-as-scott]

~~~
func fold<S:Viewl, R>(zero: R, combine: (S.El, R) -> R)(subject: S) -> R {
    if let (x, xs) = subject.uncons() {
        return combine(x, fold(zero, combine)(subject: xs))
    } else {
        return zero
    }
}
~~~
{: .langauge-swift}

[^fold-as-scott]: For completion, it's worth noting that the Church encoding can be naturally, if verbosely, produced from the Scott encoding `func fold<S:Viewl, R>(z: R, f: (S.El, R) -> R)(subject: S) -> R { return scott(z, { (x, xs) in f(x, fold(z, f)(subject: xs)) })(subject: subject) }`

## Handling infinite data types

Now, finally, we can get to the real meat of this whole thing. I
hinted above that not all implementors of `Viewl` are particularly
obviously containers. One particularly important example is the
potentially infinite `Stream`. In fact, `Stream`s are in some sense
the canonical or universal implementors of `Viewl`.

That's a little mystical, though. To be more clear, I mean to say that
`Stream`s are literally nothing more than frozen `uncons`es. Check it
out!

~~~
// (This doesn't actually compile yet)
struct Stream<A>:Viewl {
    typealias El = A
    
    let project: () -> (A, Stream<A>)?

    func uncons() -> (A, Stream)? {
        return project()
    }
}
~~~
{: .language-swift}

In other words, a `Stream` is *completely defined* by the action of
`uncons`. In this sense `Stream` is the universal implementor of it.

Except, I lied quite a bit with this implementation, since we need to
go through some indirection via `Ptr` again to avoid compiler bugs.
Let's annoint this and make a real `Stream` implementation

~~~
struct Stream<A>:Viewl {

    typealias El = A
    
    // Internal use only---use the initializer to write a more natural
    // unfold which ignores the use of Ptr
    let project: () -> (A, Ptr<Stream<A>>)?

    // Convenience constructor! Makes it so that we don't have to use
    // Ptr to construct Streams
    init(p: () -> (A, Stream<A>)?) {
        project = {
            if let (a, s) = p() {
                return (a, Ptr(s))
            } else { return nil }
        }
    }
    
    // Just unwrap the Ptr!
    func uncons() -> (El, Stream)? {
        if let (x, a) = project() {
            return (x, a.val)
        } else { return nil }
    }

}
~~~
{: .language-swift}

We can demonstrate the power of the *universal `Viewl`* by turning
literally any other type which conforms to `Viewl` into a `Stream`

~~~
func leftStream<S:Viewl>(s: S) -> Stream<S.El> {
    return fold(
      Stream.empty(),
      { (x,xs) in return Stream { return (x, xs) } }
    )(subject: s)
}
~~~
{: .language-swift}
