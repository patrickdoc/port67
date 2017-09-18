# Types in C

After last time, I hope that you are starting to get a feel for what types are.
They are a set of rules for interpreting the raw binary data as something else.
I am now going to present some of the standard types of C, and talk about their
rules.

The `printf` function will help us do some interesting things here. `printf` is
used to print and format strings. Formatting gives us the ability to insert the
value of variables into the string, but types play a role in how they are
printed. In general, `printf` can be given a string with a number of format
specifiers, and a variable for every format specifier. For example,

```
#include <stdio.h>

int main(int argc, char **argv) {
    int x = 0;
    printf("The value of x is: %d\n", x);
}
```

Within the quotes, we have our string argument. The format specifier is `%d`
which prints the variable as if it is an integer. As I introduce each type, I'll
also show how to print it. At the end, we will try printing with the wrong type
to see what will happen.

We will also use `sizeof` to check how many bytes these types actually use.
When C was first developed, computers could hold much less data than they can
now. As a consequence, the C standard only sets minimum guidelines for how big
different types should be. Specific examples below should help.

## char

A `char` is a single byte encoded using ASCII. To set a `char`, it is best to
use a single character with single quotes `'a'`.

```
#include <stdio.h>

int main(int argc, char **argv) {
    char x = 'a';
    printf("The value of x is: %c\n", x);
    printf("x takes up %d byte(s)\n", sizeof(x));
}
```

As we can see, a `char` just uses a single byte of space.  This means it can
represent 256 different values.

## unsigned

`unsigned` is the standard decimal translation we have been using.

```
#include <stdio.h>

int main(int argc, char **argv) {
    unsigned x = 0;
    printf("The value of x is: %u\n", x);
    printf("x takes up %d byte(s)\n", sizeof(x));
}
```

Here people's results may be different. The C standard promises that an
`unsigned int` (or equivalently `unsigned`) is at least 2 bytes. In the earliest
stages of C, computers could only handle 16 bits of information at the same
time. If we divide by 8 bits because there are 8 bits in a byte, we will see
that this is 2 bytes. So some of you will have 2 byte `unsigned`'s that
represent the range 0 - 65536.

Figuring out ranges is usually pretty easy. In decimal, we know that the biggest
4 digit number is 9999, and when we add 1, we get 10000. It's the same in
binary. The biggest 8 bit number is `1111 1111` and when we add 1 we get `1 0000
0000`. Remembering how we translate to decimal, this is just 2^8 = 256.
Subtracting 1, we know the biggest 8 bit number is 255. Likewise, the biggest 16
bit or 2 byte number is 2^16 - 1 = 65535

Since the invention of 32-bit and more commonly know 64-bit computers,
`unsigned`'s are more often 4 bytes in size. This makes their range of values
0 - 4,294,967,295 (or 2^(32) - 1). Even though 64-bit would allow for 8 bytes
(8 bits * 8 bytes = 64 bits) C just stuck to 4.

## int

`int`'s are the signed version of `unsigned int`'s. This is how we represent
negative numbers. The size is the same, and the range just shifts halfway into
the negatives, -32,768 - 32,767 for 2 byte `int`'s or -2,147,483,648 -
2,147,483,647 for 4 bytes.

```
#include <stdio.h>

int main(int argc, char **argv) {
    int x = 0;
    printf("The value of x is: %d\n", x);
    printf("x takes up %d byte(s)\n", sizeof(x));
}
```

## float

Floating point numbers are numbers that look like `1234.52` or `-4.234234234` or
`1234901234.515156114`. These numbers are meant to represent the rational
numbers (often written as fractions). However, one difficult property of the
rational numbers is that given any two, there are an infinite number of
rationals between them. If you give me `0.3` and 0.4`, I can start by listing
`0.31`, 0.311`, `0.3111`, and keep going forever. Floating point numbers have a
lot to cover!

```
#include <stdio.h>

int main(int argc, char **argv) {
    float x = 0;
    printf("The value of x is: %f\n", x);
    printf("x takes up %d byte(s)\n", sizeof(x));
}
```

Working with a byte again for simplicity, floating point numbers have 3 parts: a
sign bit, an group of exponent bits, and a group of fraction bits. The number of
bits in the exponent and fraction depends on the implementation. In real
`float`'s, there are 8 exponent bits and 23 fraction bits. Let's say for our
purpose that there are 4 exponent bits and 3 fraction bits.

```
                            1101 0110
                sign: 1     exponent: 1010      fraction: 110
                        sign * fraction^exponent
                        -1   * 110 e (1010)
                        -1   * 6  * 10^(-6)
                            -0.000006
```

It's not essential that you know exactly how floating point works, but it is a
pretty amazing idea. In practice, you typically won't have to worry about
implementation details of this or other types. However, it is good to know, and
if you continue deeper into computer science, you will likely need the knowledge
at some point.

## Printf Fun

I promised we would mess with some wrong types, so here we go. Let's think about
this piece of code,

```
#include <stdio.h>

int main(int argc, char **argv) {
    float x = 97.0;
    printf("x takes up %d byte(s)\n", sizeof(x));
    printf("The (float) value of x is: %f\n", x);
}
```

To us, the conversion to an `int` is clear, just drop the numbers after the
decimal. Let's try just telling `printf` to print `x` as if it were an `int`.

```
#include <stdio.h>

int main(int argc, char **argv) {
    float x = 97.0;
    printf("x takes up %d byte(s)\n", sizeof(x));
    printf("The (float) value of x is: %f\n", x);
    printf("The (int) value of x is: %d\n", x);
}
```

```
$ gcc types.c
$ ./a.out
x takes up 4 byte(s)
The (float) value of x is: 97.000000
The (int) value of x is: 1720696848
```

Unfortunately, that did not go as we hoped. The issue is that we told `printf`
we were going to give it an `int`, but we gave it a `float` instead. Now, we can
give it `x` as an `int`, we just have to specify it. The way I wrote the
`printf` output actually hints at how we do it.

In C we can "cast" types. To "cast" `x` as an `int`, we do this: `(int) x`. This
tells C that it needs to read `x` as if it were an `int` instead of whatever it
is. Let's try properly casting `x`.

```
#include <stdio.h>

int main(int argc, char **argv) {
    float x = 97.0;
    printf("x takes up %d byte(s)\n", sizeof(x));
    printf("The (float) value of x is: %f\n", x);
    printf("The (int) value of x is: %d\n", x);
    printf("The casted (int) value of x is: %d\n", (int) x);
}
```

```
$ gcc types.c
$ ./a.out
x takes up 4 byte(s)
The (float) value of x is: 97.000000
The (int) value of x is: 1720696848
The casted (int) value of x is: 97
```

Great! I want to point out that we didn't change `x` at all. Casts only change
the way the value is read in that exact place. If we use `x` again it will still
be a float.

How about `char`?

```
#include <stdio.h>

int main(int argc, char **argv) {
    float x = 97.0;
    printf("x takes up %d byte(s)\n", sizeof(x));
    printf("The (float) value of x is: %f\n", x);
    printf("The (int) value of x is: %d\n", x);
    printf("The casted (int) value of x is: %d\n", (int) x);
    printf("The casted (char) value of x is: %c\n", (char) x);
}
```

```
$ gcc types.c
$ ./a.out
x takes up 4 byte(s)
The (float) value of x is: 97.000000
The (int) value of x is: 1720696848
The casted (int) value of x is: 97
The casted (char) value of x is: a
```

As we (maybe) expected, ASCII maps the byte `0110 0001` to `a`, so the output is
`a`.

I want to end on a word of warning. As I promised, C gives us all kinds of
power. However, I hope you can get a glimpse of how wrong this all can go. Type
casting is dangerous business. But, it is a tool that you will have to use and
it is best that you have some idea of how it works. Programming is all about
choosing the best tool for the job, so you need to know about everything you have
access to.

[arrays]()
