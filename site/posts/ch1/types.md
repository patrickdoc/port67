---
title: "Types"
---

We now have a system in place for converting 1's and 0's to regular decimal
numbers. We can write a small C program to experiment with this. Open a new file
with Vim, name it whatever you would like as long as it ends with `.c`, and copy
or type this program into it.

```c
#include <stdio.h>

int main(int argc, char** argv) {
    int x = 0b01000001;
    printf("Integer: %d\n", x);

    return 0;
}
```

Compile and run this to see what the program does. I named my file `types.c`.

```bash
$ cc types.c
$ ./a.out
Integer: 65
```

There are a few new features in the code. First, we stored a binary value into
the variable `x`.

```c
int x = 0b01000001;
```

The `0b` prefix tells the compiler that this is a binary number, not one million
and one. This line defines the variable `x` to have the binary value `0100
0001`. Later on in our program, if we use the variable `x`, it will be replaced
by its value.

The second new feature is `printf`.

```c
#include <stdio.h>
...
printf("Integer: %d\n", x);
```

`include` tells the compiler that there is code somewhere else on the computer
that we would like to use. In this case, we would like to use `printf` from the
file `stdio.h`, short for "Standard Input/Output". `printf` allows us to output
text to the terminal, which you saw when you ran the program.

`printf` is a function. The first argument, `"Integer: %d\n"`, describes what we
want to print to the terminal. The `%d` is a placeholder. It will be replaced by
the decimal value of the next argument to `printf`. In this case, the next
argument is `x`.

So when our program runs, it prints `Integer: <decimal value of x>`. If you
convert the byte `0100 0001` to decimal, you should find that it is `65`, just
as our program printed out.

Let's now tweak our program.

```c
#include <stdio.h>

int main(int argc, char** argv) {
    int x = 0b01000001;
    printf("Integer: %d\n", x);
    printf("Character: %c\n", x);

    return 0;
}
```

We added another `printf` with a new placeholder. This time we used `%c` to
print the "character" or letter value of `x`. If we compile and run our program,

```bash
$ cc types.c
$ ./a.out
Integer: 65
Character: A
```

as you maybe expected, we again see that the computer thinks `65` and `A` are
the same.

## Ascii

In RAM, we have the byte `0100 0001`. We know how to use binary to interpret
that byte as a regular decimal number. However, we've seen a few times now that
the computer will also interpret that exact same byte as `A` in some cases. So
there must be another system that converts bytes into letters.

There are many systems out there because computers have to support so many
different languages. However, most systems are compatible with ASCII, the
"American Standard Code for Information Interchange". That means that most
systems agree `0100 0001` is `A`.

ASCII is just a table that maps bytes to letters. You can search for "ascii
table" on the internet and find the full mapping, but you can also use the
program above to try out specific values.

For each of the values below, update the program to have `int x = <val>;`,
compile, and run.

```
'A'
'B'
'Z'
97
'b'
```

If you play with this enough or look at the table, you might notice that the
capital and lowercase letters are all in the correct order. This allows you to
do something silly like,

```c
int x = 'A' + 1;
```

Try to guess what the result of that will be and then run it.

##

Binary and ASCII are only two of the possible mappings from data stored in RAM
back to meaningful data. We call these mappings "types", and they allow us
program without thinking about exactly how our data is mapped into memory.

There are a few more "primitive" data types in C. These are the types built-in
to C that are always available for you to use. If you think carefully about
binary, you might notice that we don't have negative numbers. Every byte value
is translated to a positive decimal number.

We also don't have fractional numbers, like `1.2` or `3.5`. Those are called
"floating point" numbers and are quite tricky to map. One major difficulty is
that there are an infinite number of floating point numbers between any two
numbers. For example, we can always add a `1` to the end of `1.11111` and get a
new value. It is not easy to fit an infinite amount of numbers into a finite
amount of memory.

Here are some of the basic C types and the systems used to map the values into
memory. You rarely need to know the particulars of each system, but if you are
curious you should be able to find more information online for all of these.

Type        System          Example Values
----        ------          --------------
char        ASCII           'A', 'B', 'c', '}'
unsigned    Binary          1, 2, 452
int         2's Complement  1, -1, 600, -1000
float       Floating Point  1.2, 0.0002, 100.8
