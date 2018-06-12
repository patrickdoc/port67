---
title: Haskell's Basic Types
prev: 2-usage.md
next:
...

# Haskell's Basic Types

Just like we did with C, let's do a review of Haskell's basic types. I'm going
to throw you in the deep end, and you can see how you do.

```haskell
module Types where

-- Boolean negation         (Pattern Matching)
notMatch :: Bool -> Bool
notMatch True = False
notMatch False = True

-- Boolean negation with different definition       (If-Then-Else)
notIf :: Bool -> Bool
notIf b = if b
    then False
    else True

-- Convert a tab to a space, but leave others alone     (Case)
tabToSpace :: Char -> Char
tabToSpace c = case c of
    '\t' -> ' '
    _    -> c

-- Unit, the simplest function
unit :: ()
unit = ()

-- 16th power                                           (Let-In)
sixteenth :: Int -> Int
sixteenth x = let fourth y = y * y * y * y
              in fourth x * fourth x

-- Square even numbers, leave others alone              (Where)
squareEven :: Int -> Int
squareEven x = if isEven x
    then x * x
    else x
  where
    isEven i = i % 2 == 0
```

Pattern Matching/Case

We can define functions pointwise. That is, specify for each input what the
output should be. `x` matches anything and stores it in the variable `x` (or
`c`, etc.). `_` matches anything, but ignores the value

Let/Where

This is how we create local variables. Haskell is very limited in creating
variables.
