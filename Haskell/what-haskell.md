---
title: What is Haskell
prev:
next:
description:
...

# What is Haskell?

A variety of adjectives are used to describe programming languages.
Unfortunately, people are often inaccurate or imprecise in their descriptions.
Haskell is often referred to as a "lazy, functional language," but that is
neither correct, nor the whole picture.

It can actually be more useful to think of these descriptions as pointing to
what Haskell is not. It is not strict; just because you tell it to do something
does not mean it will do it right then. It is not imperative; a Haskell program
is not a sequence of instructions, but a description of a function.

Even this is not satisfactory, however, because you can write imperative style
code in Haskell and functional style code in C. Let's drop these descriptions
and come at it from a different angle. What problem is Haskell trying to solve?
There must be a reason people have put time and energy into developing Haskell
when there are plenty of other good languages.

I propose that the answer is this: if we give up some power (e.g. raw memory
access or global variables) we can eliminate entire classes of bugs. For
example, if I only let you have addition, you never have to worry about going
past the end of an array. You don't have arrays! Obviously that language would
be limited in what it could do, so from there, you add more functionality while
making sure not to introduce bugs. If you want to add a feature to the language,
all you have to do is prove that it is safe.

By sacrificing some features, we gain others. You can prove facts about Haskell
programs just by looking at the types! Inside a Haskell function that takes an
`Int` and returns an `Int`, you can't modify a global variable, delete someone's
hard drive, or launch missiles. You only get to work with that integer. Think
about this expression: `(1 + 2 + 3) * 0`. In C, it would do all the addition,
and then multiply by 0. Haskell programs can just skip the addition because they
know that multiplying 0 by anything is always 0 *and* nothing else will happen
inside the additions. A more interesting example is: `(1 + 2 + ... ) * 0`. That
is, sum up all numbers and then multiply by zero. That would be an infinite loop
in C, but can be written in Haskell to return 0.

Most of these wins come at compile time. With restrictions in place, GHC can
look at your code and modify it to be simpler. C compiler's can do this as well,
but GHC can do more. It does so much that people often say "if it compiles, it
works." This is not always true, but there are many bugs that GHC catches
because of the extra restrictions it puts on you.

Simon Peyton Jones, one of the central figures in the Haskell world, once joked
that Haskell is safe, but not useful. On the other side, you have C, which is
useful but not safe: [here](https://www.youtube.com/watch?v=iSmkqocn0oQ). I've
found that this is a pretty good perspective on why things are the way they are.
In C, you can do anything you want. A fair amount of the time, you accidentally
create bugs that you have to be careful to avoid. C is very useful, and you are
providing the safety. In Haskell, you have to follow a strict set of rules. You
have to find a way to write your program while following all of these rules.
Haskell is safe, and you are providing the usefulness. The ideal language is
both safe and useful. Haskell is just approaching that ideal from a different
perspective.

When working with any language, ask yourself if you could have avoided that bug.
Or, even better, if the compiler could have spotted that bug for you. That is
how languages improve. You run into some error, find some way to always avoid it
in the future, and then teach people/languages how to avoid it too. This is a
hard problem and we'll take all the help we can get!
