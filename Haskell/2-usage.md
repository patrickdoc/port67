---
title: Using Haskell
next:
prev: 1-getting-started.html
description:
...

# Using GHC and Haskell

Haskell is a bit like C and Python combined. It has both an interpreter and
compiler. Let's start with the Python style coding. First, we can open up the
interpreter called GHCi.

## Like Python

```bash
$ ghci
```
```haskell
Prelude> 1 + 2
3
Prelude> let circumference r = 3.14 * r
Prelude> circumference 1
3.14
Prelude> circumference 2
6.28
Prelude> 
```

Just like Python, we can now move this to a file to resuse our work.

```haskell
-- Circumference.hs
module Circumference where

-- This is a type annotation. It is not always necessary,
-- but you really should always do it.
circumference :: Int -> Int
circumference r = 3.14 * r
```

```bash
$ ghci Circumference.hs
```
```haskell
*Circumference> circumference 1
3.14
```

And just like Python, we can interpret the file and run it.

```haskell
-- Circumference.hs
module Circumference where

main :: IO ()
main = print (circumference 1)

-- This is a type annotation. It is not always necessary,
-- but you really should always do it.
circumference :: Int -> Int
circumference r = 3.14 * r
```

```bash
$ runghc Circumference.hs
3.14
```

So we have the same tools as we had in Python. But now we will see that we also
have the same tools we had in C.

## Like C

To compile we need to tell GHC that this is the main module of the program.

```haskell
-- Prog.hs
module Main where

main :: IO ()
main = print 10
```

```bash
$ ghc Prog.hs
[1 of 1] Compiling Main     ( Prog.hs, Prog.o)
Linking Prog ...
$ ./Prog
10
```

And that's it, for a quick review:

Haskell | Python | C
ghci    | python |
ghci Lib.hs | python -> import Lib.py |
runghc Prog.hs | python Prog.hs |
ghc Prog.hs | | gcc Prog.c
