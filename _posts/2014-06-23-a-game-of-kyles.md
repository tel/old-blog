---
layout: page
title: A Game of Kyles
---

Here's a nim-alike game. Given a row of tokens you and I alternate
taking either one or a pair of touching tokens. The last person to
take a token wins. For example, if I begin, my turns marked by an
arrow from the left, the game of 5 tokens might proceed.

```
me -> XXXXX
      XX XX <- you
   -> X  XX
      X  X  <-
   -> X
            <- (I win)
```

If we call this game `5`, for 5 tokens all together, we might call the
game begun on the second line `[2, 2]`, for two games of 2 played
together. It's clear that by the rules one move may not affect two
simultaneous games simultaneously, though the existence of other games
may affect your strategy.

We'd like to know whether or not a given game is winnable. Since the
two sides are symmetric, we'll just write `win B` if the board `B` is
winnable by the first player. We'll also write `loss B` if `not (win
B)`.

---

As in any alternating game like this, the tit-for-tat strategy
executed by the second player is a guaranteed loss for the first.
Formally, let's call a board symmetric if we can pair off the
simultaneous games: `[2,2]` is symmetric while `[2,3,2]` is not. We
immediately have that `symmetric B |- loss B`.

As a corollary, any board which is one move removed from symmetric is
a win—the first player makes a move resulting in a symmetric board and
then plays tit-for-tat from then on to win. An important example of
such a board is the contiguous board, `|- win [n]`. Thus our starting
example of the game `5` being a win is now immediate.

If we have a symmetric board `B` and another symmetric board `C`, then
the combined board `B || C` is still symmetric. Likewise, if we have a
symmetric-once-removed board `D` then `B || D` is still
symmetric-once-removed. Thus we can see immediately that `[2,3,2]` is
the combination of the symmetric board `[2,2]` and the contiguous
board `[3]`. Thus `[2,3,2]` is still symmetric-once-removed and thus
we have `win [2,3,2]`.

<!-- Even more powerfully, if we have a board which can be decomposed as `B -->
<!-- || C` for a symmetric board `C` then `win (B || C)` iff `win B`. We -->
<!-- can prove this by induction on the size of `C`. -->

<!-- 1. If `C` is `[]` then we have `win B` iff `win B`. -->
<!-- 2. Otherwise, we can choose our move as taking a token from either `B` -->
<!--    or `C`. Let's first choose `C`. -->
<!--    * Our opponent could respond using the tit-for-tat strategy. This -->
<!--      gives us a new situation `B || C'` where `C'` is two tokens -->
<!--      smaller than `C` but still symmetric. We apply the inductive -->
<!--      hypothesis. -->
<!--    * Our opponent could pick a token from `B`. -->
<!--      * If `win B` holds then now we've got the chance to play `B' || -->
<!--        C'` where `loss B'` holds and `C'` is symmetric-once-removed. -->
<!--        We choose to play tit-for-tat on `C'` then our opponent holds -->
<!--        `B' || C''` where `C''` is symmetric and by the inductive -->
<!--        hypothesis we'll have shown `win (B || C)`. -->
<!--      * If `loss B` holds then now we've got the chance to play `B' || -->
<!--        C'` where `win B'` holds and `C'` is symmetric-once-removed. -->
<!--        * If we play appropriately for `B'` and the opponent sees `B'' -->
<!--          || C'` where `loss B''` and `C'` is symmetric-once-removed. If -->
<!--          they play tit-for-tat on `C'` then we'll have `B'' | C''` and -->
<!--          have a loss by the inductive hypothesis. -->
<!--        * If we play tit-for-tat on `C'` then the opponent sees `B' || -->
<!--          C''` and wins by the inductive hypothesis. -->
<!--        *  -->
