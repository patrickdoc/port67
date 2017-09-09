# Fun Types

Let's play with the simple byte `0110 0001`. We need to give this number some
meaning. To the computer, it is just a sequence of voltages, but if we want to
get anywhere, we need to interpret this as something.

A good place to start is as a decimal number. Most people are far more practiced
in doing math base 10, so being able to represent those numbers seems like a
good idea. Well, the standard way to read numbers is to break them down like we
did last time. I'm going to shorten the process slightly to save space, but feel
free to walk through it using the previous lesson as a guide.

```
                        0  1     1  0    0001
                          2^6 + 2^5    +   2^0
                           64 +  32    +    1
                                    97
```

Ok, so if we interpret this number as a decimal number, we get 97. Let's start a
little chart to keep track of all our interpretations. Just like types, we are
going to specify our interpretation before the variable, so we can keep
everything straight.

```
binary  x = 0110 0001
decimal x = 97
```

Now, we usually read our numbers as if the smallest part of the number was on
the left, but what if we reversed that. So instead of counting `8, 9, 10` we
counted `8, 9, 01`. We will simply read the number backward.

```
                        0  1     1  0    0001
                          2^1 + 2^2    +   2^7
                           2  +  4     +   128
                                    134
```

So we expand our table:

```
binary   x = 0110 0001
decimal  x = 97
backward x = 134
```

Numbers are all fine, but what about letters? Can we come up with a way to
read a byte as some combination of letters? Certainly! There are a million ways
we could do it. (You may have heard of two of them, called Unicode and ASCII).
Let's come up with a rule. How about every 0 becomes `a` and every 1 becomes
`z`. We would get,

```
binary   x = 0110 0001
decimal  x = 97
backward x = 134
az       x = azza aaaz
```

That's pretty similar to the original though, and we are missing most of the
letters. What about something more interesting? Well, we could use letters like
numbers. We could "count" `a,b,c,...,y,z,aa,ab,ac` and so on. If we pair up the
letters with the numbers 0-25, we can do this somewhat easily.

```
0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
a b c d e f g h i j  k  l  m  n  o  p  q  r  s  t  u  v  w  x  y  z
```

```
97 / 26 = 3 remainder 19 -> t
19 / 26 = 0 remainder 19 -> t
```

```
binary   x = 0110 0001
decimal  x = 97
backward x = 134
az       x = azza aaaz
count_a  x = tt
```

ASCII and Unicode are ways of interpreting bytes as letters. The way they work
though is to specify a single character for every bytes. You can see a chart
HERE. If we do some quick math, we can see that the biggest number a byte can
hold is `1111 1111` or 255. So we could theoretically store 256 different
charactes in our chart. In ASCII, `0110 0001` (or 97) corresponds to `a`.

```
binary   x = 0110 0001
decimal  x = 97
backward x = 134
az       x = azza aaaz
count_a  x = tt
ascii    x = a
```

From just a single byte `0110 0001`, we have generated all kinds of possible
information. Just by reading it differently, we can store any data we want. Some
of the methods I used were not very practical. `az` probably does not have any
uses in the real world because it is effectively identical to binary. On the
other hand, ASCII is used non-stop around the globe.

Next time we'll see some of the actual types used in C. Unfortunately they are
slightly less fun than my examples.

[8-c-types]()
