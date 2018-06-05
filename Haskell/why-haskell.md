---
title: Why Haskell
next:
prev:
description:
...

# Why Haskell?

I have already shown you two programming languages, so you may be wondering why
I am presenting a third. There are many ways in which learning a wide variety of
languages can help you, but I'll answer from the perspective of the purpose of
this site. I am teaching you Haskell so that I can more easily teach other topics.

Take memory for example. With C, I can write programs that simply and clearly
manipulate the heap and the stack. I can show you why certain tricks work and
why arrays start with index 0. If I tried to do the same in Python, I would have
had to leave standard, basic Python and do more complicated things. In C, memory
manipulation is just a part of programming.

So why Haskell? Well Haskell is intended to be an abstract or "high-level"
language. That means you (usually) won't have to worry about managing memory.
You can compare strings with `==`. You actually have boolean values
`True/False`. And you will never segfault.

If we don't have to worry about these issues, we can focus on whatever task is
at hand. It's much easier to work with strings when you don't have to worry
about memory. At the same time, we can talk about how Haskell actually works.
Everything we learned from C will let us peek under the hood at Haskell, making
us better Haskell programmers.

## GHC and GHCi

There is presently only one compiler for Haskell that has widespread acceptance:
the Glasgow Haskell Compiler (GHC). There are a few that have specific uses, but
for general Haskell programming GHC is the only one that is used. It can be easy
to confuse Haskell and GHC. For clarity, I will try to only use GHC when I am
speaking specifically about the compiler.

Before writing any Haskell code, GHC already provides us with a win over Python
and C. It has *both* a compiler and an interpreter. Working in an interpreter is
often convenient because it lets you check ideas without working them into an
entire program.

Here is a short example in the interpreter,

```bash
$ ghci
GHCi, version 8.2.1: http://www.haskell.org/ghc/  :? for help
Prelude> 1 + 2
3
Prelude> reverse "hello"
"olleh"
Prelude> <Ctrl-D>
Leaving GHCi.
```

This is vaguely reminiscent of Python. Here is the most basic file you can
compile.

```bash
$ echo 'main = print "Hello, world!"' > Hello.hs
$ ghc Hello.hs
$ ./Hello
"Hello, world!"
```

Compilation will also produce `Hello.o` and `Hello.hi`, which you can safely
delete.

I've deliberately kept these as short as possible because Haskell programs and
expressions are a little different than what you are used to. Next time, we'll
look at the features you are familiar with.
