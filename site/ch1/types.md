---
title: "Primitive Types"
---

We can now translate from the hardware into binary, and from binary into
decimal. But, while numbers are useful, we also want to work with other data.
One of the simplest data types we want to work with is text. We need a way to
represent letters in memory.

The most common system for converting text to binary is called ASCII. ASCII is
simply a table that maps bytes to characters. Characters include capital
letters, lower case letters, digits, and punctuation. Here's a snippet of the table,

Binary       Character
------      -----------
0011 0000       0
0011 0001       1
0011 0010       2
0011 0011       3
...
0011 1010       :
0011 1011       ;
0011 1100       <
...
0100 0001       A
0100 0010       B
0100 0011       C
...
0110 0001       a
0110 0010       b
0110 0011       c

There are some interesting patterns in the table, but mostly it is just an
arbitrary definition. The basic ASCII table only defines the first 128 values.
[^1] Over time, new standards have been established that cover more of the
characters used around the world. But most of them are compatible with ASCII,
meaning that they match on the first 128 values.

[^1]: If you are looking closely, you'll notice that 128 is a power of 2.
  Strangely, it is only 7 bits of data, even though our standard byte is 8 bits.
  Text encoding just happens to be older than the 8 bit standard.

## Types

When we wrote our first program, we saw a surprising result. To refresh your
memory, it looked like this,

```c
int main(int argc, char** argv) {
    return 'A';
}
```

```bash
$ cc test.c
$ ./a.out
$ echo $?
65
```

We told the program to return the letter 'A', but instead it returned the
number 65. If we use our knowledge of binary, decimal, and ASCII, we now know
enough to explain the result.

Binary      Decimal   Character
-------    --------- -----------
0100 0001      65         A

In memory, the number 65 and the letter 'A' both correspond to the same byte,
the exact same set of charged and uncharged circuits in the hardware. The only
difference is how we choose to read that byte. If we read it as a binary
number, the value is 65. But if we read it as an ASCII character, the value is
the letter 'A'.

ASCII and binary are only two of the ways that we can read bytes of data. If you
think carefully about binary, you might notice that we don't have negative
numbers. Every byte is translated to a positive decimal number. We also don't
have fractional numbers, like `1.2` or `3.5`. Those are called "floating point"
numbers and are quite tricky to represent.

These data types that we read straight from memory are called "primitive" types.
They are built-in to C, so we don't need to worry about exactly how they work.
But we do need to communicate to the compiler which type it should use to read
the data in memory.

We communicate this information to the compiler through the "type system". Types
are how we work with the compiler to manage memory. We provide the type, and the
compiler takes care of reading and writing the data in memory.

Here are some of the primitive types in C.

Type        System          Example Values
----        ------          --------------
char        ASCII           'A', '9', 'c', '}'
unsigned    Binary          1, 2, 452
int         2's Complement  1, -1, 600, -1000
float       Floating Point  1.2, 0.0002, 100.8

`char` indicates that a value should be read as an ASCII letter, and `unsigned`
indicates that a value should be read as a binary number. The two new ones are
`int` and `float`. `int` is short for "integer", or positive and negative
numbers. `float` is short for "floating point" which includes the rational
numbers.

When we write programs, we tell C which type to use for a specific variable. We
"declare" a variable to have a type when we create it, like this,

```c
int x = 10;
char myVariable = 'c';
float otherVariable = 1.0;
```

C is not very strict with types though, so we can write code that looks like
this,

```c
int x = 'A';
char myVariable = 10;
```

But by doing that, we are losing most of the value that types provide.

##

Primitive types are useful, but they only represent single values. Most useful
data is composed of more than just one number or letter. If you sign up for an
account on a website, you typically have to put in at least your name and email,
both of which have multiple characters in them. You might also need to input
your birthday or phone number which, again, are more than a single value.

The final tool we need is the ability to build new types out of the primitive
types. Once we can do that, we can represent and manipulate any type of data we
want.
