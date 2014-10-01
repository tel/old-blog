---
layout: post
title: Papers We Love BOS No. 2, "Foundations of Logic and Functional Programming via Martin-Löf"
comments_enabled: true
---

[I gave a talk](https://www.youtube.com/watch?v=xRUPr322COQ&feature=youtu.be)
for
[Papers We Love, Boston](http://www.meetup.com/Papers-We-Love-Boston/events/203312152/)
on the 18th where I discussed Martin-Löf type theory from a historical
and programming context. The goal was to motivate and inspire reading
the (notably long) 1983 Martin-Löf lectures
[*On the Meanings of the Logical Constants and the Justifications of the Logical Laws*](https://github.com/michaelt/martin-lof/blob/master/pdfs/Meanings-of-the-Logical-Constants-1983.pdf?raw=true). [The video](https://www.youtube.com/watch?v=xRUPr322COQ&feature=youtu.be) was just posted and the slides [are available here](http://jspha.com/public/slides/PapersWeLoveBOS_Sept19th_MartinLof_Intuitionism_Programming.pdf).

The hosts, [Ashley Williams](https://twitter.com/ag_dubs) and
[Bob Holt](https://twitter.com/bobholt) from
[Bocoup](http://bocoup.com/) were absolutely wonderful and I really
appreciate Bob's work editing and producing
[the video](https://www.youtube.com/watch?v=xRUPr322COQ&feature=youtu.be).

All in all, it was a great experience and I'm really appreciative of
everyone who helped put the talk together, who are building Papers We
Love, and, most of all, everyone who attended, asked questions, and
continued after the talk with great conversation.

Thanks so much!

---

Thanks to correspondance, some errors have been discovered.

* [Tom Crockett](https://twitter.com/pelotom) pointed out that my
  example described
  [at minute 46](http://www.youtube.com/watch?v=xRUPr322COQ&t=46m0s)
  *is* actually the intuitionistically valid form of proof by
  contradiction of "reductio ad absurdum". I explored the idea of
  proving $$\neg A$$ from the premise $$\neg\neg\neg A$$, which is
  intuitionistically valid, but spoke as though I was considering
  proving $$A$$ from $$\neg\neg A$$ which isn't. In other words, we
  can manufacture counterexamples using contradiction, but not
  demonstrate proofs. So, my example there is totally wrong and should
  be talking about trying to prove something by assumption that it
  can't be incorrect.

  Thanks, Tom!

  For completeness, here is a proof term for $$p : \neg\neg\neg A \to
  \neg A$$, $$p\ (k : \neg\neg\neg A)\ (a : A) = k\ (\lambda (p : \neg A)\ .\ p a)$$

