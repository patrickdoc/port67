---
title: "Binary"
---

The smallest unit of RAM is a single circuit that either has an electrical charge
or doesn't. When it has a charge, the computer reads the value as 1. When there
is no charge, it is read as 0. To write a value into memory, the hardware has to
charge and discharge these cells to correspond to the correct order of 0's and
1's.

RAM provides the ability to store 1's and 0's, but we are responsible for
providing the meaning of these values. Our regular system of counting,
"decimal", uses the digits 0-9. But since we don't have 2-9 available, we need a
slightly different system.

We will be using the "binary" number system. It may look different at first, but
it is reasonably similar to decimal. Here are the first 10 numbers in each
system,

```
Decimal: 0  1   2   3    4    5    6    7     8     9    10
Binary:  0  1  10  11  100  101  110  111  1000  1001  1010
```

Two comparisons might help you understand how binary works. First, notice that
when we count, we use up all of our single digits, and then add 1 to the left
and restart. In binary we just run out of digits faster, so we have to start new
columns faster.

Second, notice that 10 and 2 play crucial roles in determining the value of a
number. Specifically, we can do this in decimal,

```
                            432
=           4 x 100   +    3 x 10   +   2 x 1
=           4 x 10^2  +   3 x 10^1  +  2 x 10^0
=             400     +      30     +     2
=                           432
```

Let's break down a binary number similarly.

```
                            101
=             1 x 4   +    0 x 2    +   1 x 1
=            1 x 2^2  +   0 x 2^1   +  1 x 2^0
=               4     +      0      +     1
=                            5
```

In school, you might have been taught about the "1's place", the "10's place,"
and the "100's place". In binary, you might call them the "1's place", "2's
place", the "4's place" and so on. Each time you can multiply by another copy of
the base (2 or 10) to get your value.

Because computer science uses binary so heavily, powers of 2 are useful to know.
For reference,

```
          2^0 2^1 2^2  2^3   2^4  2^5  2^6 2^7 2^8 2^9 2^10 2^11 2^12
Decimal:    1   2   4    8    16   32   64 128 256 512 1024 2048 4096
Binary:     1  10 100 1000 10000 10^5 10^6 ...
```

You don't need to memorize this chart, but keep an eye out for powers of 2.
They show up regularly in Computer Science.

## Bits and Bytes

Binary is easy for a computer to read, but hard for most people to read. To help
with that, we often break it up into groups of 4. So instead of `11010`, we
write `0001 1010`. You can check that adding 0's to the left doesn't change the
value of the number by multiplying and then adding like we did above.

We also have names for these groups to help discuss them. A single 0 or 1 of
binary is called a "bit", short for "binary digit". A group of 8 "bits" is
called a "byte", the standard measure of data in Computer Science.[^1]

[^1]: Sometimes people call groups of 4 "nibbles", but I think that
is mostly just for a good laugh. I told you CS people love their name jokes.

So now you should be able to figure out what `65` looks like as a byte of binary
data. We just need to figure out why the computer thinks `65` is the same as the
letter `A`.
