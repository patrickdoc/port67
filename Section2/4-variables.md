---
title: Variables
section: Section2
prev: 3-main-breakdown.html
next: 5-functions.html
...

# Variable Declarations

Working with variables in C has a few more rules than in Python. Take this
program for example,

```c
int main(int argc, char **argv) {
    float x;
    x = x + 1;
    return 0;
}
```

The declaration of `x` is something that I glossed over last time. In C, you
not only have to give types to your functions, but also your variables. Here
I am declaring `x` to be a variable of type `float` without giving it a value.
One of the "gotcha"s of C is that it does not stop you from using variables
with no value. The most dangerous part is that this behaviour is "undefined".
This means that the compiler (`gcc`, `clang`, or any other) does not have to
ensure that something reasonable occurs.

The first option is to initialize the variable at declaration. You can declare
and set `x` in one fell swoop,

```c
float x = 3.14;
```

If you don't know the value you want `x` to have at that time, you can always
set it to some default and then check for that default before you use it.

```c
float x = 0;

// A bunch of code...

if (x == 0) {
    // We need to set x
    x = 3.14;
}

// Code that now uses x
```

## Compiler Warnings

The second solution to this problem is to use the `-Wall` flag to your compiler.
This flag tells the compiler to warn you about every place in your code that
something fishy is happening. Modern compilers are actually quite good at
spotting bugs like these, so I would recommend *always* using `-Wall`. As
an example, let's look at this program that uses an undefined `x`.

```c
// undefined_x.c

int main(int argc, char **argv) {
    float x;
    x = x + 1;
    return 0;
}
```

Compiling without and with `-Wall`,

```bash
$ gcc undefined_x.c
$ gcc -Wall undefined_x.c
undefined_x.c:5:9: warning: variable 'x' is uninitialized when used here
      [-Wuninitialized]
    x = x + 1;
        ^
undefined_x.c:4:12: note: initialize the variable 'x' to silence this warning
    float x;
           ^
            = 0.0
1 warning generated.
```

Without `-Wall` we get no warnings. With `-Wall`, we get loads of useful
information. Reading compiler output is sometimes easy and sometimes a bit
misleading. I will try to run you into many of these warnings so you get
practice with them.

```bash
undefined_x.c:5:9: warning:
```

This piece is extremely useful. It tells you
`file_name:line_number:character_number` where the warning is. If you open the
file with `vim`, you can jump right to that line by typing `:line_number`. In
our case, `:5`.

```bash
warning: variable `x` is uninitialized when used here
```

Perfect, it caught exactly the error we hoped it would. I used `x` without
initializing it to any value. Then I tried to use it, even though I had never
given it a value.

```bash
undefined_x.c:4:12: note: initialize the variable 'x' to silence this warning
    float x;
           ^
            = 0.0
```

The compiler went above and beyond here to offer a solution to our problem. And
notice, it was the first option I gave above. These suggestions are not always
great, but they are usually good starting points for fixing your code.

```bash
1 warning generated.
```

As a quick summary, we see that there was only 1 warning generated when reading
our code. This number can sometimes be inflated when a single error causes a
large number of warnings. Finding the root of your problems just takes
practice.
