# C Types

With our newfound powers of binary, let's get our hands dirty with some types.

C's type system is quite explicit, in that you have to label everything with
a type, but very weak. It has very few safeguards to prevent you from doing
bad things. In fact, you can always ask C to pretend that a variable has a
different type. This is called a "type cast". We are going to use `printf` to
interpret `x` in various different ways.

First, let's choose some numbers to work with. I'm going to choose 0, 10, and
97. In binary, that would be,

```
 0:  0000 0000
10:  0000 1010
97:  0110 0001
```

It is customary to write binary in block of four, similar to how a number like
`1 000 000` is broken up into blocks of three. When we normally write numbers,
we start with the first non-zero digit. However, we technically *could* include
zeros before the first digit. For example, `100` is the same as `000100`. This
is never really useful in normal math though, except maybe when teaching
addition. In computer science though, we often include the leading zeros to
more closely match the representation in the computer.

We call a single digit (i.e. `0` or `1`) a "bit" in binary. A block of eight
like I wrote above is called a "byte". If you know your metric prefixes, then
it is now clear what a "kilobyte", "megabyte", or "gigabyte" is. It is
simply 1000, 1,000,000, or 1,000,000,000 of these "bytes" all lined up next to
each other. Doing some quick binary translation, a "byte" can hold all of the
values from 0 to 255. That's a fair amount of information for such a small
package!

The first conversion we are going to make is to hexadecimal. Hexadecimal is the
name for base 16 math, and it turns out to be a useful way to write binary. If
we look at a block of four "bits" (sometimes called a "nibble"... I told you
these name jokes would come back) , it can encode exactly 16 values, 0-15.
When we run out of digits in hexadecimal, we switch over to letters. So couning
looks like this,

```
0 1 2 3 4 5 6 7 8 9 a b c d e f 10
```

The great thing about hex is that each letter corresponds to exactly on
"nibble".

```
0: 0000
1: 0001
2: 0010
3: 0011
4: 0100
5: 0101
6: 0110
7: 0111
8: 1000
9: 1001
a: 1010
b: 1011
c: 1100
d: 1101
e: 1110
f: 1111
```

Let's pick 3 numbers and convert them by hand before checking with C.

```
decimal: 0   binary: 0000 0000   hex: 0
decimal: 10  binary: 0000 1010   hex: a
decimal: 97  binary: 0110 0001   hex: 61
```

With all that out of the way,

```c
int main(int argc, char **argv) {

    // Our variables we will play around with
    int x = 0;
    int y = 10;
    int z = 97;

    // Casting 'x' to different types
    printf("x in hexadecimal: %x\n", x);
    printf("y in hexadecimal: %x\n", y);
    printf("z in hexadecimal: %x\n", z);

    return 0;
}
```
```bash
$ gcc -Wall types.c
$ ./a.out
x in hexadecimal: 0
y in hexadecimal: a
z in hexadecimal: 61
```
