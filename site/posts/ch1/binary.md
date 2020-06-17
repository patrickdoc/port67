---
title: "1's and 0's"
---

We've seen a little bit about how memory is used and the differences between
volatile and non-volatile memory, but we haven't see much about what memory is.
When a file is "written" into memory, what does that translate to on the
hardware?

At the smallest scale, a single cell of memory is an electic circuit that can
either be charged with electricity or empty. When we write to memory, we
adjust the state of this circuit by either charging it or discharging it.

You can think of the cell like a light switch, either in the "on" or "off"
position. When writing, we can flip the switch, and when reading, we can check
if the switch is currently on or off. Instead of "off" and "on" though, we
typically use `0` to represent the discharged state and `1` to represent the
charged state.

We call a single cell of memory a "bit", short for "binary digit". Digits are
the numbers 0 to 9, but "binary" limits us to only the first two, 0 and 1.

If our memory only had one bit, there would only be two possible
configurations, `0` or `1`. But if we add a second bit, we have four possible
configurations shown below.

Bit 1 | Bit 2
0 | 0
0 | 1
1 | 0
1 | 1

If we add a third bit, we double the number of configurations again for a total
of eight.

Bit 1 | Bit 2 | Bit 3
0 | 0 | 0
0 | 0 | 1
0 | 1 | 0
0 | 1 | 1
1 | 0 | 0
1 | 0 | 1
1 | 1 | 0
1 | 1 | 1

Each time we add a new bit of memory, we double the number of possible
configurations. By the time we reach 8 bits, we have 256 different combinations
of 0's and 1's available to us.

A group of 8 bits forms a "byte", which is our standard unit of memory. Like
other scientific units, we can use prefixes like "kilo-", "mega-", and "giga-"
to represent one thousand, one million, and one billion bytes respectively. The
computer I am working on has 8 gigabytes of memory, which translates to 8
billion bytes or 64 billion bits.

Bytes are the only data that memory understands. When we want to write a new
letter into a file, it must first be translated into bytes before it is written
into memory. When we want to open a file, the bytes in memory must be converted
back to letters before we can read them.

## Binary

Depending on the data we are trying to translate, we use a few different
systems. The first system we will look at is "binary". Binary is a system of
counting that only uses 0's and 1's. Our usual system, "decimal", uses all the
digits 0 through 9. Despite missing out on 2-9, binary is completely equivalent
to decimal, and we can always translate from one system to the other.

Binary may look different at first, but it is reasonably similar to decimal.
Here are the first 11 numbers in each system,

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
and the "100's place". In binary, you could call them the "1's place", "2's
place", the "4's place" and so on. Each time you can multiply by another copy of
the base -- 2 or 10 -- to get your value.

Because computer science uses binary so heavily, powers of 2 are useful to know.
For reference,

```
          2^0 2^1 2^2  2^3   2^4  2^5  2^6 2^7 2^8 2^9 2^10 2^11 2^12
Decimal:    1   2   4    8    16   32   64 128 256 512 1024 2048 4096
Binary:     1  10 100 1000 10000 10^5 10^6 ...
```

You don't need to memorize this chart, but keep an eye out for powers of 2.
They show up regularly in Computer Science.
