---
title: Binary
...

# Why Types?

Although we have talked about types, first in Python and now in C, I haven't
really explained what they are or why we have them. To get at that, we need to
jump down all the way to the bottom of our abstraction layers and talk about the
hardware of the computer. This is probably the lowest we will ever go. In fact,
we need to briefly talk about some electrical engineering to understand how
computers work.

## Circuits

The various pieces of hardware inside your computer all communicate through
electrity. If you look up a picture of a motherboard (particularly the
underside), you will see lines snaking across the entire surface. These are
effectively wires connecting every component. Electricity flows through these
wires carrying information.

This information is encoded as either high or low voltage. If you haven't taken
a physics class on electricity, then you can think of voltage like pressure.
When a large amount of liquid is forced through a pipe, it will come out on the
other side forcefully. On the other hand, if only a small amount is travelling
down the pipe, it will come out as just a trickle. Someone on the receiving end
of this current could check every second and record the pressure. They might
write down "high, high, low, high, low, low...' and so on.

This process is very similar to telegrams and Morse code. In fact, the principle
is the exact same. People wanted to transfer information using electricity.
There is a very simple system that matches up well with these "high"'s and
"low"'s called binary. We use binary to jump the barrier between the physical
electricity and the ideas we want to put in the computer.

(This is an unsatisfactory summary of the way circuits work, but without
assuming at least high school physics it is the best I can do. See GOOD CIRCUIT
SOURCE for more details if you are interested.)

## Binary

Binary is a system of numbers consisting of only 0's and 1's. Hopefully you can
see why it pairs up well with the "low"'s and "high"'s of electricity. Our 1's
translate to "high"'s and our 0's to "low"'s.  The system you are more familiar
with is called decimal. It is a "base-10" number system, while binary is
"base-2".

Binary may look pretty different at first, but I am going to try and convince
you that it is reasonably similar to decimal. Here are the first 10 numbers in
each system:

    0  1   2   3    4    5    6    7     8     9    10
    0  1  10  11  100  101  110  111  1000  1001  1010

There are two comparisons that I think might help you understand how this
works. First, notice that when we count, we use up all of our single digits, and
then add 1 to the left and restart. In binary we just run out of digits faster,
so we have to start new columns faster.

The second observation is that 10 and 2 play crucial roles in determining the
value of a number. Specifically, we can do this:

                                432
    =           4 x 100   +    3 x 10   +   2 x 1
    =           4 x 10^2  +   3 x 10^1  +  2 x 10^0
    =             400     +      30     +     2
    =                           432

Let's break down a binary number similarly

                                101
    =             1 x 4   +    0 x 2    +   1 x 1
    =            1 x 2^2  +   0 x 2^1   +  1 x 2^0
    =               4     +      0      +     1
    =                            5

In school, you might have been taught about the "10's place" and the "100's
place". In binary, you might call them the "2's place" and the "4's place" and
the "8's place" and so on. Each time you can multiply by another copy of the
base (2 or 10) to get your value.

Because computer science uses binary so heavily, powers of 2 are *really* good
to know. For reference,

    2^x:        0  1   2    3     4    5    6   7   8   9   10   11   12
    in decimal: 1  2   4    8    16   32   64 128 256 512 1024 2048 4096
    in binary:  1 10 100 1000 10000 10^5 10^6   ...

I don't mean to overwhelm you with a big chart of numbers, but I wanted to try
and give you a sense of why they are important. 1000 counting in binary is 2^3,
while in decimal it is 10^3. This is what we mean by "base 10" or "base 2". It
is the base of the exponent.

These will come up a bunch, so you'll start to memorize them. But try to keep
an eye out for these so you know where they come from.

## Bytes

Binary becomes unreadable pretty quickly, just look at this number `11010110`.
In decimal, it is only `214`.  Because of this, it is standard to break up
binary into groups of 4, `1101 0110`. While not simple to read, it does make it
a little bit easier. We also frequently fill in 0's on the left to make the
number fill up a whole group.  For example, we might write `11101` as `0001
1101`. You can check that it is always safe to add 0's to the left by breaking
down the number like we did above.

A single 0 or 1 of binary is called a "bit" as opposed to the decimal "digit". A
group of 8 "bits" is called a "byte". (Sometimes people call groups of 4
"nibbles", but I think that is mostly just for a good laugh. I told you CS
people love their name jokes.) Bytes are the single most important measure in
computer science.

If you know your metric system prefixes, then "gigabytes" and "megabytes" should
instantly jump out at you. If you don't, then "giga" means 10^9, or one billion,
and "mega" means 10^6, or one million. Thus a gigabyte is one billion bytes, or
eight billion bits. The numbers get out of hand very fast. Modern smartphones
hold billions and billions of bytes. And they fit in your pocket!

I realize we are quite a distance from the idea of types that we started with.
We just need one more piece to figure this out: memory.

# Next
[Memory](7-memory.html)
