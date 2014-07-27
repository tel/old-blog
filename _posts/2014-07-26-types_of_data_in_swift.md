---
layout: post
title: The Types of Data (in Swift)
comments_enabled: true
---

One of the most exciting parts about Apple's new language Swift is
that is has great representation of the
[Types of Data](http://tel.github.io/2014/07/23/types_of_data/). Let's
see what they look like.

### Unit, the useless type

This type is built into Swift if you remember that $$1$$ is a whole
lot like an empty tuple.

~~~
let someUnit: () = ()
~~~
{: .language-swift}

but we can also define it ourselves using an `enum`

~~~
enum Unit { case T }
~~~
{: .language-swift}

### Void, the unobtainable

This type is, so far as I'm aware, not built into Swift, but we can
generate it by using a tricky cheat.

~~~
enum Void { case void( Void ) }
~~~
{: .language-swift}

This cheat works because it forces us to make an infinite loop in
order to build a value of `Void`, and so we can *try* to construct one
but it'll take, literally, forever. We abuse this property in order to
get `Void`'s "unlimited" usage.

~~~
func absurd<A>(v: Void) -> A {
    switch v {
    case .void(let v): return absurd(v)
    }
}
~~~
{: .language-swift}

### Sums, choices of construction

Sums are represented in Swift as the various `enum` members. Thus, we
can build the basic binary sum called $$\_ + \_$$ pretty easily.

~~~
enum Sum<L, R> {
  case Inl(L), Inr(R)
}
~~~
{: .language-swift}

So we construct sums using `Sum.Inl` and `Sum.Inr` and we can use a
standard `switch` statement to use sums. We can also package this use
up into a function.

~~~
func sum<L, R, X>(left: L -> X, right: R -> X)(s: Sum<L,R>) -> X {
    switch s {
    case .Inl(let l): return left(l)
    case .Inr(let r): return right(r)
    }
}
~~~
{: .language-swift}

### Products, collections of constructions

Products are obviously Swift tuples. We can also use an `enum` again
to build the basic binary product.

~~~
enum Product<L,R> { case Product(L, R) }
~~~
{: .language-swift}

Now we construct products using `Product.Product` and can build a
destructor like

~~~
func product<L,R,X>(p: Product<L, R>)(s: Sum<L -> X, R -> X>) -> X {
    switch (p, s) {
    case (.Product(let l, _), .Inl(let f)): return f(l)
    case (.Product(_, let r), .Inr(let g)): return g(r)
    }
}
~~~
{: .language-swift}

### Exponentials, implications, hypotheticals, functions

Finally, by this point, it should be obvious that function spaces are
just Swift functions. They're constructed using `func` or anonymous
`{... in ...}` syntax and destructed by applying an argument using
some parentheses.

## Conclusion

When Swift was first announced I was super excited to see that the
designers were giving Swift such great first-class support for
constructing and eliminating the types of data. I'd be happy to claim
that basically all languages are shaped by the types of data, but it's
a mark of a language with great abstraction facilities that the types
of data are so approachable.

Hopefully this post serves as a companion for Swift programmers to
quickly see how the types of data appear in Swift.
