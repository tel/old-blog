---
layout: post
title: Typing Transducers (as State Machine Transformers)
comments_enabled: true
---

[Previously][typing-transducers-1] I analyzed Clojure's new
transducers feature by considering them as mappings between reducers
of type `forall r . (b -> r -> r) -> (a -> r -> r)` which,
interestingly, showed up as being identical to the Kleisli list arrow
`a -> [b]`.

Unfortunately, this representation lacks a notion of "reduction local"
state and thus cannot capture transformations like `take`. In Clojure
this is not a big problem since you can use Clojure's ambient
mutability to regain state via `atom`s. In Haskell this isn't an
option and we have to seek out other avenues.

[typing-transducers-1]:http://tel.github.io/2014/08/10/typing-transducers/

If the type of a reducer is `
