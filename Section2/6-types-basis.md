# Purpose of Types

We have seen a bit of types in both C and Python now. Languages all handle types
slightly differently, but most use them to some extent. It is important to make
the distinction between the "syntax" of a programming language (i.e. the
structure of written code) and the more central ideas of the language. In
Python, types aren't really emphasized at all in the syntax. They are in the
background until you make a mistake by trying to add an `int` and a `string` for
example. That doesn't mean Python doesn't use types; Python is just designed to
be more approachable on the surface.

As we have seen so far in C, types are all over the syntax. You need them for
function declarations and variable declarations and can explicitly work with
them, which we will get to soon. In fact, there are places where C requires us
to change the types of variables.

Before we get deeper into the use of types though, it makes sense to take a
moment to explain what they are. To do that, we will need to learn about binary.

## Binary

Falling all the way to the bottom of our layers of abstraction, a computer runs
on electricity flowing through the circuit boards. The use of the word "flowing"
is actually reasonably helpful in understanding the big trick of computing.
Electricity in a wire moves similarly to water in a pipe.

One of the properties of electric currents that we can measure is the voltage.
Very roughly, this can be compared to pressure in a pipe. The various components
of the circuitry can communicate by varying the voltage of the electric current.
Just like the long and short beeps or flashes of light in Morse code, we only
care about two kinds of pressure, high and low.

Instead of long and short, we use 1 and 0. 1's indicate high voltage and 0's
low. With that, we can ignore the physical layer for a while and move up the
layers of abstraction.

(That is a highly unsatisfactory description of how circuits work. Without
assuming everyone has taken high school physics, that is about the best I can
do. For those who want more information, see: GOOD CIRCUITRY SOURCE. For now
though, this explanation should suffice.)

So what does this have to do with binary? Binary is a base 2 number system. That
means it only has two digits in it, 0 and 1. As we have just discovered, this is
a very good way to communicate with the computer. We send it 0's and 1's, and it
can adjust the voltage accordingly.

Which leads us to, how does it work? Typically when we count, we say 0, 1, 2,
... 8, 9, 10, but in binary we don't have the numbers 2 through 9. I'm going to
compare binary and decimal (the system you are used to) and then walk through
the differences.

```
     0  1   2   3    4    5    6    7     8     9    10
     0  1  10  11  100  101  110  111  1000  1001  1010
```

There are two comparisons that I think might help you understand how this
works. First, notice that when we count, we use up all of our single digits, and
then add 1 to the left and restart. In binary we just run out of digits faster,
so we have to start new columns faster.

The second observation is that binary is a base 2 system, whereas decimal is a
base 10 system. In more concrete terms we can do this:

```
                                    432
        =           4 x 100   +    3 x 10   +   2 x 1
        =           4 x 10^2  +   3 x 10^1  +  2 x 10^0
        =             400     +      30     +     2
        =                           432
```

Let's break down a binary number similarly

```
                                    101
        =             1 x 4   +    0 x 2    +   1 x 1
        =            1 x 2^2  +   0 x 2^1   +  1 x 2^0
        =               4     +      0      +     1
        =                            5
```

Because computer science uses binary so heavily, powers of 2 are *really* good
to know. For reference,

```
decimal: 1  2   4    8    16   32   64 128 256 512 1024 2048 4096
2^x:     0  1   2    3     4    5    6   7   8   9   10   11   12
binary:  1 10 100 1000 10000 10^5 10^6   ...
```

I don't mean to overwhelm you with a big chart of numbers, but I wanted to try
and give you a sense of why they are important. 1000 counting in binary is 2^3,
while in decimal it is 10^3. This is what we mean by "base 10" or "base 2". It
is the base of the exponent.

These will come up a bunch, so you'll start to memorize them. But try to keep
an eye out for these so you know where they come from.

## Bytes

Binary becomes unreadable pretty quickly. Consider this number `11010110`.
Because of this, it is standard to break up binary into groups of 4, `1101
0110`. While not simple to read, it does make it a little bit easier. We also
frequently fill in 0's on the left to make the number fill up a whole group.
For example, we might write `11101` as `0001 1101`. It is always safe to add 0's
to the beginning of a number. You can check this for yourself by breaking down
the number like we did above.

A single 0 or 1 of binary is called a "bit" as opposed to the decimal "digit". A
group of 8 "bits" is called a "byte". (Sometimes people call groups of 4
"nibbles", but I think that is mostly just for a good laugh. I told you CS
people love their name jokes.) Bytes are the single most important measure in
computer science.

If you know your metric system prefixes, then "gigabytes" and "megabytes" should
instantly jump out at you. If you don't, then "giga" means 10^9, or one billion,
and "mega" means 10^6, or one million. Thus a gigabyte is one billion bytes, or
eight billion bits. The numbers get out of hand very fast. Modern smartphones
hold billions and billions of bytes. And they fit in your pocket.

The trick is turning a byte like `0100 0111` into something usable. It just so
happens that types are our answer for this.

[Types]()
