---
layout: page
title: A Game of Kyles
---

Consider a two-player game called Kayles. The rules are that we have a
line of tokens and take turns removing either a single token or two
adjacent ones trying to be the person to remove the last token, the
winner.

For instance, here is a game where the first player wins.

```
me -> XXXXX
      XX XX <- you
   -> X  XX
      X  X  <-
   -> X
```

This game is well-studied and solvable as an impartial summable game.
While I know that now thanks to reading a bit of the marvelous book
*Winning Ways for your Mathematical Plays* by Berlekamp, Conway, and
Guy, originally I did not.

Here's another equivalent game. Two players argue over stacks of
tokens, on their turn splitting the stacks trying to be the one to
remove the final token. On each players turn they (1) pick one stack,
(2) remove either 1 or 2 tokens, and then (3) have the option of
partitioning the remainder into two stacks each of 0 or more tokens.

The number of possible moves is linear in the size of the stack *n*
(it's the number of 2-partitions of *(n-1)* and *(n-2)*, *floor
((n-1)/2) + floor ((n-2)/2) + 2*) and each move reduces *n* by at most
2. The size of the game tree is exponential and brute forcing the game
tree will likely fail. We can exploit symmetry to help, though.

---

A game is symmetric if every stack has a sibling of identical size,
thus *{2 2 4 4 4 4}* is symmetric. Symmetric games are lost by the
first player to play by an obvious strategy—the second player merely
imitates the first. The empty game *{}* is symmetric and a loss for
the first player. Further, given any symmetric game, the imitation
strategy returns to the first player *another* symmetric game and so
by induction the first player has lost.

Trivially, any single stack of tokens, any "singleton" game, is won by
the first player. If the stack is even then the first player takes two
tokens from it and gives a symmetric game to the second player. If its
odd, then only one token is taken.

We'd also like to see games as compositions of other games. We can add
two games like *G + H* to mean the game *G* played simultaneously with
the game *H*. Obviously, this is the structure of all games of
Kayles—they're all sums of singleton games. This suggests the, perhaps
obvious, point that winning in Kayles happens in the interaction of
the stacks. If they didn't interact then every game would be trivial.

---

To regroup: we're playing the game Kayles which consists of singleton
games corresponding to natural numbers, sizes of stacks of tokens, and
their sums. Each player may, on their turn, pick a stack and reduce it
by either 1 or 2 tokens then split it into two substacks of 0 or more
tokens. A game that can be decomposed as *G + H + H* is called
symmetric.

We'd like to say whether the first player can win the game, *G*. We'll
write that as *win G*, otherwise we'll write *loss G = not (win G)*.
We also talk about the possible moves from some game and write that
for any *m* in *moves(G)* that *mG* is *G* after *m* has been
performed.

With these mechanics, we can give an inductive definition to *win* and
*loss*. We have, by definition, *loss {}*. We also have that *win G*
holds if there exists an *m* in *moves(G)* such that *loss mG*.
Symmetrically, this gives us that *loss G* holds iff all *m* in
*moves(G)* have *win mG*.

---

It'd be nice to be able to decompose games into simpler equivalent
games. We'll talk about this tacitly, though if you're being technical
we might talk about equivalence classes of games under the predicate
*win*. We say *G* is equivalent to *H*, or *G ~ H* where *win G* iff
*win H*.

Since symmetric games evaporate down to the empty game *{}* and adding
the empty game to any other game is an identity, we might prove the
following theorem:

> Any game *G* is equivalent to *G + H* if *H* is symmetric.

There's a mild proof of this involving induction on the size of games.
The size of a game *G*, called *sz(G)*, is the number of tokens in all
of *G*'s stacks.

So, we'll prove that *symmetric H* implies *G ~ G + H* by induction.
The induction is on *sz(G + H) = n* and gives us the induction
hypothesis that for all *symmetic H'* and all *G'* such that *sz(G' +
H') < n* we have *G' ~ G' + H'*. So now given *G* and *symmetric H*
such that *sz(G + H) = n* and *sz(H) = m* we prove that *G ~ G + H* in
two parts.

We'll begin by proving that *win G* implies *win (G + H)*. All we have
to do is find a move *m* in *moves (G + H)* such that *lose m(G + H)*
holds, but *win G* implies there's a move *m* in *moves(G)* such that
*lose mG* holds. If we apply this move *m(G + H) = mG + H* and we can
use the induction hypothesis to see that *lose mG* implies *lose (mG +
H)*.

Now we prove that *win (G + H)* implies *win G*. We use *win (G + H)*
to find a move *m* in *moves(G + H)* such that *lose m(G + H)*. Since
moves can correspond to only one stack and stacks can be in only one
game, there are two possibilities: either *m* applies to *G* or *m*
applies to *H*.

In the first case, *m(G + H) = mG + H* which the induction hypothesis
tell us is a loss iff *mG* is, thus *loss mG* holds and so does *win
G*.

In the second case, *m(G + H) = G + mH* and since *loss (G + mH)*
holds we know that all *n* in *moves(G + mH)* have *win n(G + mH)*.
Since *H* is symmetric, one of those moves is *m'*, the immitation
move of *(m, H)*, so *n(G + mH) = G + nmH* and the induction
hypothesis says *G + nmH* is equivalent to *G*, so *win (G + nmH)*
implies *win G*. QED.

And armed with this lemma, if we can find a decomposition of *G* into
*G' + H* for symmetric *H* then we have *G ~ G'*.

---

Right now we regard a game *G* as a sum of singleton games where a
particular singleton game might occur many times, however any even
subset of those singletons is a symmetric game and can be removed.

To be more clear, all games of Kayles are equivalent to subsets of the
naturals. We can convert the sum-of-singletons view by counting the
occurrence of *n*-sized stacks in the sum. If *count(n, G)* is the
number of times that *n*-sized stacks occur in *G* then we can convert
games to the new representation with the mapping

    G --> { n | n in Nat, count(n, G) is odd }

We can also say that *win* is a predicate of the type *win : 2^2^Nat*.

A final representation of *win* is that of a predicate on binary
strings, like *Bool\* -> Bool*.
