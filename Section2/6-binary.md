# Binary

One of the fundamental issues in computer science is how to represent data.
What we type into our files and interpreters is eventually converted into
electricity. As this is not a course on electrical engineering, we are going to
do away with that bottom layer. The next form the data takes (going higher up
the stack, or more "abstractly") is binary.

## Binary

This topic comes up in a variety of topics, but here is probably the best place
to introduce it. The electric current flowing through the circuits of the
computer have a property called voltage that can be measured. Very roughly,
this can be compared to pressure in a pipe. Using this pressure, all the parts
of the circuit board can communicate with each other. We can encode this using
binary.  1's represent high pressure, and 0's low.  With that, we will leave
that bottom layer behind and treat binary as the lowest level of our stack.

(That is a highly unsatisfactory description of how circuits work. Without
assuming everyone has taken high school physics, that is about the best I can
do. For those who want more information, see: GOOD CIRCUITRY SOURCE. For now
though, this explanation should suffice.)

So what is binary? As I mentioned before, it is entirely composed of 0's and
1's. Typically when we count, we say 0, 1, 2, ... 8, 9, 10, but in binary we
don't have the numbers 2 through 9. I'm going to compare binary and decimal
(the system you are used to) and then walk through the differences.

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
```

Let's break down a binary number similarly

```
                                    101
        =            1 x 4    +    0 x 2    +   1 x 1
        =            1 x 2^2  +   0 x 2^1   +  1 x 2^0
```

Because computer science uses binary so heavily, powers of 2 are *really* good
to know. For reference,

```
1 2 4 8 16 32 64 128 256 512 1024 2048 4096
0 1 2 3  4  5  6   7   8   9   10   11   12
```

These will come up a bunch, so you'll start to memorize them. But try to keep
an eye out for these so you know where they come from.

## Interpreting Binary

So now that we know about binary, we can talk about how many different types
of data are stored.

We start with something easy, numbers. As we saw above, we can convert any
binary number into decimal by simply multiplying by powers of two and adding it
all up. The reverse can be done by repeatedly dividing by 2 and writing down
the remainder. (We'll come to negative numbers later).

But something terrible has just happened that you probably did not notice.
We've used up every single binary number, making them all represent decimal
numbers. So how do we store other data, like letters? To solve this, we have
a notion of types. Types can be thought of as a mapping from binary to anything
else!

Throughout the development of computer science, we have come up with several
of these maps from letters to numbers. We call them "encodings", and you may
have even heard of some of the famous ones like ASCII or Unicode. ASCII is
simpler, but cannot handle many of the characters used around the world. And so
Unicode was made to handle a larger number of them. But as these things go, even
the original Unicode specification didn't cover all of them, so more had to be
made.

ASCII defines the letter "A" to be mapped to 65 (or 0100 0001 in binary. It is
traditional to separate binary every fourth digit, much like we add commas to
1,000,000). We need a differnt code for lowercase "a", which turns out to be
97 (or 0110 0001). And similarly, all the other characters you typed are
mapped to numbers.

So what is 0110 0010? A good inital idea is to convert this number to decimal.
If you did that, you would find it is equal to 98. Thinking about our discussion
of character encodings, you might notice that this is one greater than "a", so
you might suggest "b". The answer is the terribly unsatisfying: it depends.

As a programmer, you will need 97 to be many things. Sometimes it will have to
be "a". Other times you will need it to just be the number 97. And perhaps once
you might need it to be the representation of a particular banana. But even
though to you it is all these things, to the computer it will always be
voltage corresponding to 0110 0001.

We as programmers, with the help of programming languages, are in charge of
properly assigning meaning to this data. One of the great strengths of Python
(and also a weakness) is that it hides much of this complexity from us.
Consider this small piece of Python code:

```Python
x = 1
y = 'a'

xplus = x + x
yplus = y + y
```

Try to guess what the values of ``xplus`` and ``yplus`` are. For the first
case, it is relatively straightforward. ``1 + 1 = 2``. (Practice adding in
binary by hand to make sure that it works. Just remember to carry the 1.) But
figuring out what ``'a' + 'a'`` means is less clear. The people who made Python
decided that the best thing to do would be to make this work in a reasonable
way. In this case, ``yplus = 'aa'``. Now if we treated this the same way as
``xplus``, we would end up with whatever the ascii value of 97+97 is. But it
seems unlikely that the average programmer wants that to be the outcome.
Instead they guessed that people probably wanted the characters to be stuck
together. A reasonable guess.

Next time, a brief encyclopedia of types.

[Interpreted]()
