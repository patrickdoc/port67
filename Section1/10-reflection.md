---
title: A Reflection
section: Section1
prev: 9-control-flow.html
...

# Reflecting on Python

What we have seen so far of Python is by no means the whole language. Yet it
is more than enough to write interesting programs. We are going to drop Python
for a bit and move on to C. But before we do that, I want to lay out some of
the pros and cons of Python. This will help us understand the differences
between the two languages.

## Age

Probably the biggest source of differences between Python and C comes from
their age gap. C was created in the early 70's, while Python was released in
the early 90's. Twenty years is a long time in computer science. Not only did
the theory behind programming languages grow considerably, but the power of the
hardware drastically changed. One measure of computing power is the number of
transistors in your computer. These are pieces of hardware responsible for
controlling the electric current inside your computer. At the time C was
invented, computers had ~2,300 transistors. When Python was released, they had
crossed 1,000,000. Since then, we have passed 10,000,000,000.

While these numbers are not exact measures of the ability of computers, they
do give a sense of the rapid growth in technology. Smartphones are more
powerful than the mainframes that businesses used to use, and those were so big
that rooms had to be specifically built for them. One of the most impressive
feats of C is living through all this and still being relevant. No programming
language has stood the test of time quite as well as C. Python, on the other
hand, was able to learn from the mistakes of older languages to make great
design choices.

## Interpreted vs. Compiled

One major advantage that Python has over C is that Python had C to work with.
Python is just a C program. It reads the code you give to it, shuffles some
stuff around, and then runs C code. This is the difference between "interpreted"
languages and "compiled" languages. The code you run in Python has to be
processed by Python every time you run it. Whereas C is compiled into file that
the computer knows how to run. We will do this ourselves shortly.

Interpretation is both a blessing and a curse for Python. The good news is that
Python is far easier to write than C. Many of the bugs that occur in C either
cannot happen in Python or are gracefully handled and reported to you. Python
excels at creating prototypes and short scripts because it handles things in
the background so you can worry about making your program, not difficult bugs.

The downside to this is that Python trades performance for ease of use. The
steps that Python takes of reading the code and transforming it to C don't
happen when you just use C. No matter how good Python is at generating C, you
could always just write the same C yourself. Some people have gone through
enormous efforts to make Python performant, in particular, the NumPy library
for math is so highly optimized that you can get stellar performance from it.
However, there are limits to how fast you can make it.

## Syntax

For comparison, here is a Python and C "Hello, world!".

```python
def main():
    print "Hello, world!"

main()
```

```c
#include <stdio.h>

int main(int argc, char **argv) {

    printf("Hello, world!");
    return 0;
}
```

Without getting into details, it is clear that, at the very least, C is more
verbose. While not necessary, I included a `main` function in the Python
source to make the comparison slightly more fair. Two changes that immediately
stand out are the `{}`'s and `;`'s. In Python, we use indentation and
newlines to differentiate commands. In C, we have to clearly mark everything.

Also note the type indicators everywhere: `int main`, `int argc`, `char
**argv`. These are required in C. While Python mostly hides the types from
you, here we have to handle all of that ourselves. That is really the core of
the difference between Python and C. Python hides much of the work, while C
forces you to handle it yourself. As with every decision in computer science,
this is a tradeoff. Python is easier to use, C is more powerful.

## Type System

Here is another short program for comparison.

```python
x = 1
# x == 1
# type(x) == 'int'
x = 1 + 1.5
# x == 2.5
# type(x) == 'float'
```

```c
int main(int argc, char **argv) {
    int i;

    i = 1 + 1.5;
    // i == 2
    return 0;
}
```

In Python, values have types, and variables just hold values. In C, variables
have types and do whatever is necessary to convert the value into that type.
We can see that the value of `x` and `i` should both be 2.5. In Python,
`x` just changes its type to accommodate the value. But in C, `i` just
pretends 2.5 is an int because that is all `i` can hold. And so, `i == 2`.
(Also note the syntax for comments in C; it is `//` at the start of the
comment.)

While Python is nice, it is time to take off the training wheels and get our
hands dirty with C. We need to learn all the bits that Python is hiding from
us. Once we know those, we can come back to Python or any other language and
make better design choices.
