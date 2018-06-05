---
title: Familiar Haskell Features
prev:
next:
description:
...

# Familiar Features in Haskell

I am going to start building up a basic Haskell program that will look similar
to C or Python. The easiest place to start is comments.  They are just like
comments in C, but use `--` instead of `//`.

```haskell
-- I am a single line comment
```

It is traditional in Haskell to name you files starting with a capital letter,
so we'll just put the file name at the top in a comment.

Again like C, `main` must be in your program.

```haskell
-- Features.hs

main = print "Hello from main!"
```

`main` will always have the same type, so we'll add it in.

```haskell
-- Features.hs

main :: IO ()
main = print "Hello from main!"
```

Haskell puts a lot of focus on types, so they are designed to be somewhat
prominent. You read `main :: IO ()` as "`main` is of the type/has the type `IO`
action". The `()` just means that it doesn't return a value, similar to `void`
in C. Types are one of the most important expansions of features in Haskell, so
we will dive deeper into them later.

Here's a function with the same type as C's `main` function.

```haskell
-- Features.hs

main :: IO ()
main = print "Hello from main!"

-- In C, we would write this function as:
-- int cmain(int argc, char **argv) { return 42; }
cmain :: Int -> [String] -> Int
cmain argc argv = 42
```

Looking at the type of `cmain`, you might read it as "`cmain` takes an `Int`
and a list of `String`s, and returns an `Int`". If you then look at the
definition of the function, we name the first argument `argc` and the second
`argv`, just like in C.

To use `cmain`, we just need to provide it with the proper arguments.

```haskell
-- Features.hs

main :: IO ()
main = print (cmain 0 [])

-- In C, we would write this function as:
-- int cmain(int argc, char **argv) { return 42; }
cmain :: Int -> [String] -> Int
cmain argc argv = 42
```

Here we apply the function `cmain` to the `Int` 0 and the empty list `[]`. The
result of `cmain` is always 42, so we should expect our program to print `42`.

```bash
$ ghc Features.hs
$ ./Features
42
```

The parentheses tell the program that `print` should be applied to the result of
applying `cmain` to the two arguments. Without them, the program becomes
confused about how to combine all the pieces.

From here, Haskell gets less familiar. I need to talk about some of the ideas
behind Haskell before we dive back into code. But to give you a taste of some
of these ideas, consider the following C code.

```c
int foo(int a, int b) { return 12; }

// This is an error
foo(5)
// This is also an error
foo(5,)
```

But now look at it in Haskell.

```haskell
foo :: Int -> Int -> Int
foo a b = 12

-- This is valid Haskell
foo 5
```

There really isn't a short way of expressing what I want in C, but in Haskell
you may be able to make the jump. The question is, what is the type of `foo 5`?
