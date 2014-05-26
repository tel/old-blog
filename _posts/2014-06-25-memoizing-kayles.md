---
layout: page
title: Memoizing Kayles
---

[Consider a two-player game called Kayles](https://www.hackerrank.com/contests/lambda-calculi-may14/challenges/game-of-kyles).
The rules are that we have a line of tokens and take turns removing
either a single token or two adjacent ones trying to be the person to
remove the last token, the winner.

For instance, here is a game where the first player wins.

~~~
me -> XXXXX
      XX XX <- you
   -> X  XX
      X  X  <-
   -> X
~~~

> This game is well-studied and solvable as an impartial summable
> game. While I know that now thanks to reading a bit of the marvelous
> book *Winning Ways for your Mathematical Plays* by Berlekamp,
> Conway, and Guy, originally I did not. If you want to solve Kayles
> efficiently, study how to use the
> [nimbers](http://math.ucsd.edu/~wgarner/research/pdf/brief_intro_cgt.pdf).
> What follows is a brute force method that leads to an interesting
> memoization trick.

---

Here's another equivalent game. Two players argue over stacks of
tokens, on their turn splitting the stacks trying to be the one to
remove the final token. On each players turn they (1) pick one stack,
(2) remove either 1 or 2 tokens, and then (3) have the option of
partitioning the remainder into two stacks each of 0 or more tokens.

The number of possible moves is linear in the size of the stack *n*
(it's the number of 2-partitions of $$(n-1)$$ and $$(n-2)$$, i.e.
$$\lfloor (n-1)/2 \rfloor + \lfloor (n-2)/2 \rfloor + 2$$) and each
move reduces $$n$$ by at most $$2$$. The size of the game tree is
exponential and brute forcing the game tree will likely fail. We can
exploit symmetry to help, though.

## Symmetric games

A game is symmetric if every stack has a sibling of identical size,
thus $$\{2, 2, 4, 4, 4, 4\}$$ is symmetric. Symmetric games are lost
by the first player to play by an obvious strategy—the second player
merely imitates the first. The empty game $$\{\}$$ is symmetric and a
loss for the first player. Further, given any symmetric game, the
imitation strategy returns to the first player *another* symmetric
game and so by induction the first player has lost.

Trivially, any single stack of tokens, any "singleton" game, is won by
the first player. If the stack is even then the first player takes two
tokens from it and gives a symmetric game to the second player. If its
odd, then only one token is taken.

We'd also like to see games as compositions of other games. We can add
two games like $$G + H$$ to mean the game $$G$$ played simultaneously
with the game $$H$$. Obviously, this is the structure of all games of
Kayles—they're all sums of singleton games. This suggests the, perhaps
obvious, point that winning in Kayles happens in the interaction of
the stacks. If they didn't interact then every game would be trivial.

## Equivalences modulo symmetries

To regroup: we're playing the game Kayles which consists of singleton
games corresponding to natural numbers, sizes of stacks of tokens, and
their sums. Each player may, on their turn, pick a stack and reduce it
by either 1 or 2 tokens then split it into two substacks of 0 or more
tokens. A game that can be decomposed as $$G + H + H$$ is called
symmetric.

We'd like to say whether the first player can win the game, $$G$$.
We'll write that as $$win\ G$$, otherwise we'll write $$loss\ G = \neg
win\ G$$. We also talk about the possible moves from some game and
write that for any $$m \in moves(G)$$ that $$mG$$ is $$G$$ after $$m$$
has been performed.

With these mechanics, we can give an inductive definition to $$win$$
and $$loss$$. We have, by definition, $$loss\ {}$$. We also have that
$$win\ G$$ holds if there exists an $$m \in moves(G)$$ such that
$$loss\ mG$$. Symmetrically, this gives us that $$loss\ G$$ holds iff
all $$m \in moves(G)$$ have $$win\ mG$$.

### Symmetry elimination (a mildly laborious proof)

It'd be nice to be able to decompose games into simpler equivalent
games. We'll talk about this tacitly, though if you're being technical
we might talk about equivalence classes of games under the predicate
$$win$$. We say $$G$$ is equivalent to $$H$$, or $$G \cong H$$ where
$$win\ G$$ iff $$win\ H$$.

Since symmetric games evaporate down to the empty game $${}$$ and
adding the empty game to any other game is an identity, we might prove
the following theorem:

> Any game $$G$$ is equivalent to $$G + H$$ if $$H$$ is symmetric.

There's a mild proof of this involving induction on the size of games.
The size of a game $$G$$, called $$|G|$$, is the number of tokens in
all of $$G$$'s stacks.

So, we'll prove that $$symmetric\ H$$ implies $$G \cong G + H$$ by
induction. The induction is on $$|G + H| = n$$ and gives us the
induction hypothesis that for all $$symmetic\ H'$$ and all $$G'$$ such
that $$|G' + H'| < n$$ we have $$G' \cong G' + H'$$. So now given
$$G$$ and $$symmetric\ H$$ such that $$|G + H| = n$$ and $$|H| = m$$
we prove that $$G \cong G + H$$ in two parts.

We'll begin by proving that $$win\ G$$ implies $$win\ (G + H)$$. All
we have to do is find a move $$m \in moves\ (G + H)$$ such that
$$lose\ m(G + H)$$ holds, but $$win\ G$$ implies there's a move $$m
\in moves(G)$$ such that $$lose\ mG$$ holds. If we apply this move
$$m(G + H) = mG + H$$ and we can use the induction hypothesis to see
that $$lose\ mG$$ implies $$lose\ (mG + H)$$.

Now we prove that $$win\ (G + H)$$ implies $$win\ G$$. We use $$win\
(G + H)$$ to find a move $$m \in moves(G + H)$$ such that $$lose\
m(G + H)$$. Since moves can correspond to only one stack and stacks
can be in only one game, there are two possibilities: either $$m$$
applies to $$G$$ or $$m$$ applies to $$H$$.

In the first case, $$m(G + H) = mG + H$$ which the induction
hypothesis tell us is a loss iff $$mG$$ is, thus $$loss\ mG$$ holds
and so does $$win\ G$$.

In the second case, $$m(G + H) = G + mH$$ and since $$loss\ (G + mH)$$
holds we know that all $$n \in moves(G + mH)$$ have $$win\ n(G +
mH)$$. Since $$H$$ is symmetric, one of those moves is $$m'$$, the
immitation move of $$(m, H)$$, so $$n(G + mH) = G + nmH$$ and the
induction hypothesis says $$G + nmH$$ is equivalent to $$G$$, so
$$win\ (G + nmH)$$ implies $$win\ G$$. $$\blacksquare$$

And armed with this lemma, if we can find a decomposition of $$G$$
into $$G' + H$$ for symmetric $$H$$ then we have $$G \cong G'$$.

## Better game representations

Right now we regard a game $$G$$ as a sum of singleton games where a
particular singleton game might occur many times, however any even
subset of those singletons is a symmetric game and can be removed.

To be more clear, all games of Kayles are equivalent to subsets of the
naturals. We can convert the sum-of-singletons view by counting the
occurrence of $$n$$-sized stacks in the sum. If $$count(n, G)$$ is the
number of times that $$n$$-sized stacks occur in $$G$$ then we can
convert games to the new representation with the mapping

$$
\begin{align*}
    G \mapsto \{ n \mid \text{$n$ $\in$ $Nat$}, \text{count($n$, $G$) is odd} \}
\end{align*}
$$

We can also say that $$win$$ is a predicate of the type $$win : 2^{2^{Nat}}$$.

A final representation of $$win$$ is that of a predicate on binary
strings, like $$2^\star \to 2$$. This is particularly interesting from
a computational point of view due to the idea of lazy trie
memoization. In particular, we'd like to convert function spaces like
$$B^A$$ to lazy data structures $$A \rhd B$$ such that those two
spaces are isomorphic (at least so long as $$A$$ and $$B$$ are
finite).

## Winning as binary search

The standard source on this is Ralf Hinze's
[*Memo Functions, Polytypically!*](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.43.3272).
Briefly, we consider a recursive definition of $$A \rhd B$$ over
generic ADTs. We can then define something like $$2^\star \rhd 2$$ in
steps:

$$
\begin{align*}
    T =& 2^\star \rhd 2 \\
      =& 1 + 2 \times 2^\star \rhd 2 \\
      =& (1 \rhd 2) \times (2 \times 2^\star \rhd 2) \\
      =& 2 \times (2 \rhd 2^\star \rhd 2) \\
      =& 2 \times (2^\star \rhd 2) \times (2^\star \rhd 2) \\
    T =& 2 \times T \times T \\
\end{align*}
$$

which is clearly the type of infinite binary trees over $$2$$. This
leads to an elegant memoized implementation of "$$win$$ as binary
search". In fact, it's almost trivial to write in Haskell and quite
elegant, noting the recursive call to the memoized `win` in the
unmemoized `win'`

~~~
type Board = [Bool]

data Trie = Trie !Bool Trie Trie

moves :: Board -> [Board]

win :: Board -> Bool
win = (untrie . trie) win' where
  win' :: Board -> Bool
  win' []  = False
  win' [_] = True  -- singleton games are winnable
  win' b   = (not . all win . moves) b

untrie :: Trie -> (Board -> Bool)
untrie (Trie h _ _) []        = h
untrie (Trie _ t _) (True :n) = untrie t n
untrie (Trie _ _ f) (False:n) = untrie f n

trie :: (Board -> Bool) -> Trie
trie f = go [] where
  go b = Trie (f b)
              (go (True  : b))
              (go (False : b))
~~~
{: .language-haskell}

The only remaining tough part is defining an efficient `moves`
function that enumerates all of the possible moves given a particular
board position.
[For my own sanity, I found it easier to represent `Board` as an `IntSet` and write `moves` using that.]({{site.baseurl}}public/code/MemoKayles.hs)
This makes the `trie`/`untrie` code less obvious, but still not
terrifically complex.

And what's the end result? Well, this code must *really* be compiled
using `-O2`. Without `-02`, computing the game `[1,16]` tried my
patience, but with `-O2` this computes the game `[1,60]` in 10 seconds
on my computer. It's still nothing write home
about—[using efficient nimber mathematics reduces the runtime to milliseconds for nearly any game]({{site.baseurl}}public/code/NimberKayles.hs)—but
still an interesting way to optimize a game tree search.
