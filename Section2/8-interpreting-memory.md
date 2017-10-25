---
title: Interpreting Memory
...

# Interpreting Memory

Hopefully you've thought a bit about the problem I posed last time. I am going
to present one of the best solutions to it right now, and it always helps to
have some idea of what you are looking for when an answer it shown to you. That
way, instead of getting lost, you see answers to your problems. Consider
yourself warned...

## Unsigned Integers and 2's Complement

So far, we have been using a relatively simple method of interpreting bytes.
We've read `0010 1100` as 44, by a procedure of multiplying by powers of 2 and
summing. This is the first C type that we can get from binary. If we want a
number to be read like this, we declare:

```c
unsigned x = 10;
```

or, equivalently:

```c
unsigned int x = 10;
```

On most modern computers, `unsigned int` will be given 4 bytes. Practicing our
binary, that means they can represent any number between 0 and 4,294,967,295.
In the beginning of C, computers only understood 2 bytes at a time, so
technically, the compiler is allowed to only give you 2 bytes for your `unsigned
int`. That range is 0 to 65,535. Most of the time, you will have 4 bytes though.

But to answer our question, we are going to need a new type. We could create a
type that is just always negative. However, that type turns out to not be very
useful. It represents the same values as `unsigned`, but just uses them
differently.

Instead, we need a way to read our bits that is sometimes negative and sometimes
positive. It also makes sense to try to make half of the values negative and
half positive. It would not be good if our type ranged from -128 to 256. Keeping
track of what we can and cannot use would be a nightmare.

The type that we are looking for is C's `int`. This type is very similar to
`unsigned int`, but, as the name might imply, it has a sign. This means a value
might be +2 (but you don't have to write the `+`) or -2. It's the same size as
an `unsigned int`, but half of it's values have been shifted into the negative
range. Thus it can store any number between -2,147,483,647 and +2,147,483,647.
To achieve this, it uses a system of rules called 2's Complement.

The rules are pretty easy. We will work with 1 byte for simplicity, but it works
at any size. It starts the same as our `unsigned` rule, you multiply all the
bits by the corresponding power of 2.


    +---+---+---+---+   +---+---+---+---+
    | 1 | 0 | 0 | 1 |   | 1 | 0 | 0 | 0 |
    +---+---+---+---+   +---+---+---+---+
     128     +    16  +   8

However, you multiply the left most bit by -1. So here we end up with

    +---+---+---+---+   +---+---+---+---+
    | 1 | 0 | 0 | 1 |   | 1 | 0 | 0 | 0 |
    +---+---+---+---+   +---+---+---+---+
     -128     +    16  +   8
                -104

And that's it! That's the only change. I realize this might feel a little out of
the blue, so let's walk through some examples and examine some important
consequences.

Here is a brief chart of `int`'s. The first 3 all work the way you would expect.
The leftmost bit is 0, so the negative values don't come into play. The last 3
work the same way, except you add -128 because `-1 * 2^7 = -128`. Expand these
by hand to make sure you believe me.

    +---+---+---+---+   +---+---+---+---+
    | 0 | 0 | 0 | 0 |   | 0 | 0 | 0 | 0 | = 0
    +---+---+---+---+   +---+---+---+---+

    +---+---+---+---+   +---+---+---+---+
    | 0 | 0 | 0 | 0 |   | 0 | 0 | 0 | 1 | = 1
    +---+---+---+---+   +---+---+---+---+

    +---+---+---+---+   +---+---+---+---+
    | 0 | 1 | 1 | 1 |   | 1 | 1 | 1 | 1 | = 127
    +---+---+---+---+   +---+---+---+---+

    +---+---+---+---+   +---+---+---+---+
    | 1 | 0 | 0 | 0 |   | 0 | 0 | 0 | 0 | = -128
    +---+---+---+---+   +---+---+---+---+

    +---+---+---+---+   +---+---+---+---+
    | 1 | 0 | 0 | 0 |   | 0 | 0 | 0 | 1 | = -127
    +---+---+---+---+   +---+---+---+---+

    +---+---+---+---+   +---+---+---+---+
    | 1 | 1 | 1 | 1 |   | 1 | 1 | 1 | 1 | = -1
    +---+---+---+---+   +---+---+---+---+

## Overflow

When we were managing boxes last time, we had to come up with a response when
the program tried to store a number that required more bits than it was allowed.
At the time I suggested an error, but in practice, these types just usually just
"overflow". Compilers are often smart enough to recognize this and give you a
warning, but not always. There are even times where you can use this to your
advantage. But let's look at an example.

We have a 1-byte unsigned type called `char`. It is short for "character" and is
often used to represent letters. But, as we now know, in memory it is just
eight bits that we can interpret as we please.

```c
char x = 255;
```

                      x
    +---+---+---+---+   +---+---+---+---+
    | 1 | 1 | 1 | 1 |   | 1 | 1 | 1 | 1 | = 255
    +---+---+---+---+   +---+---+---+---+

Now, what happens when we add 1? In decimal, `255 + 1 = 256`. Unfortunately, 256
is outside of our range. It looks like this:

                      x + 1
      +---+---+---+---+   +---+---+---+---+
    1 | 0 | 0 | 0 | 0 |   | 0 | 0 | 0 | 0 | = 256
      +---+---+---+---+   +---+---+---+---+

We have a stray 1 that does not fit in our boxes! Addition in binary works just
like addition in decimal, where you carry the 1 when your column grows too
large. In this, case, we had to carry a 1 in every column, which adds a new 1 at
the beginning. But that is 9 bits, and we can only store 8.

The computer's answer to this is to simply drop the extra bit. So our example
should actually be:


                    x + 1
    +---+---+---+---+   +---+---+---+---+
    | 0 | 0 | 0 | 0 |   | 0 | 0 | 0 | 0 | = 0
    +---+---+---+---+   +---+---+---+---+

For our `char` type, `255 + 1` overflows the space available to it and resets to
0.

Let's think about how we want `int`'s to work. For the numbers -128 to 126,
adding 1 should work exactly as it does normally, that it, `-128 + 1 = -127`,
`-1 + 1 = 0`, and `126 + 1 = 127`. If we follow the pattern from `char`'s,
`127 + 1` should restart the range at -128.

The 2's complement rules handle all of this for us. Let's check various
examples:

                    0 + 1
    +---+---+---+---+   +---+---+---+---+
    | 0 | 0 | 0 | 0 |   | 0 | 0 | 0 | 1 | = 1
    +---+---+---+---+   +---+---+---+---+

                  126 + 1
    +---+---+---+---+   +---+---+---+---+
    | 0 | 1 | 1 | 1 |   | 1 | 1 | 1 | 1 | = 127
    +---+---+---+---+   +---+---+---+---+

                  127 + 1
    +---+---+---+---+   +---+---+---+---+
    | 1 | 0 | 0 | 0 |   | 0 | 0 | 0 | 0 | = -128
    +---+---+---+---+   +---+---+---+---+

                 -128 + 1
    +---+---+---+---+   +---+---+---+---+
    | 1 | 0 | 0 | 0 |   | 0 | 0 | 0 | 1 | = -127
    +---+---+---+---+   +---+---+---+---+

                   -2 + 1
    +---+---+---+---+   +---+---+---+---+
    | 1 | 1 | 1 | 1 |   | 1 | 1 | 1 | 1 | = -1
    +---+---+---+---+   +---+---+---+---+

                   -1 + 1
    +---+---+---+---+   +---+---+---+---+
    | 0 | 0 | 0 | 0 |   | 0 | 0 | 0 | 0 | = 0
    +---+---+---+---+   +---+---+---+---+

If you look closely, you can see that the type only ever actually overflows on
`-1 + 1 = 0`. The transition from `127` to `-128` is merely a result of our
rules of interpretation. 2's complement has exactly the characteristics that we
want our type to have, so we stick with it.

[C Types](9-c-types.html)
