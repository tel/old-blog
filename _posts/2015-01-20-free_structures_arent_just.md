---
layout: post
title: Free Structures Aren't Just
comments_enabled: true
---

Free structures are a popular idea in Haskell when speaking about
constructions like the "free monad" or notions like "lists are free
monoids". This terminology can be misleading, however. Free structures
aren't just "free", they have to be "free" with respect to something.

In particular, we can take the popular example of the "free monad",
often called just `Free` due to its ubiquitous value in Haskell.

~~~
data Free f a = Pure a | Free (f (Free f a))

instance Functor f => Functor     (Free f)
instance Functor f => Applicative (Free f)
instance Functor f => Monad       (Free f)
~~~
{: .language-haskell}

This `Free` type has kind `(* -> *) -> (* -> *)` and so can be seen as
taking any `Functor`, `f`, to a `Monad`, `Free f`. Thus, the most
accurate way to speak is that `Free` constructs the free monad with
respect to a functor.

As another example, we can consider the following type

~~~
data Coyoneda f a where
  Coyoneda :: f x -> (x -> a) -> Coyoneda f a

instance Functor (Coyoneda f) where
  fmap f (Coyoneda fx xa) = Coyoneda fx (f . xa)
~~~
{: .language-haskell}

This `Coyoneda` type has kind `(* -> *) -> (* -> *)` and so can be
seen as taking any type constructor of kind `(* -> *)`, `f`, to a
`Functor`, `Coyoneda f`. In other words, `Coyoneda` constructs the
free Functor with respect to a type constructor.

---

The point of the examples above is to suggest that "free" construction
is not universal but instead must be done with respect to another
structure. `Free f` "fills in" just enough structure so that `f`, a
`Functor`, can behave as a Monad.

To be even more clear, consider

~~~
type Free' f a = Free (Coyoneda f) a
~~~
{: .language-haskell}

This type has an instance `instance Monad (Free' f)` which doesn't
demand a thing of `f`. It is the free monad with respect to any type
constructor whatsoever. We can even talk about the monad instance for
something like

~~~
data Pred a = Pred (a -> Bool)

type What = Free' Pred
~~~
{: .language-haskell}

despite the fact that `Pred` cannot be made a `Functor` at all.

---

If we swing the duality hammer we can see all of this structure
arriving from the other side as well. Any free construction is left
adjoint to a corresponding "forgetful" construction. This would be the
kind of constructor which prevents us from recognizing some amount of
structure---it forgets a `Monad` into just a `Functor` or even just a
type constructor.

We can construct these forgetful mappings easily

~~~
newtype ForgetMonad f a = ForgetMonad (f a)
instance Functor f => Functor (ForgetMonad f)

newtype ForgetFunctor f a = ForgetFunctor (f a)
-- No instances
~~~
{: .language-haskell}

Here, `ForgetMonad` is a mapping which takes a monad and hides that
structure. Likewise, `ForgetFunctor` forgets so much that there isn't
even a `Functor` instance any longer. In each case, we again see that
forgetfulness is relative just like the adjoint notion of freeness.