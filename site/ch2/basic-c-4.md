---
title: "C Scope"
subFiles: []
---

When you declare variables, they are only available at certain places in the
program, called their "scope". The most basic rule is that variables must be
defined before they can be used. For example, this program will throw an error
when you try to compile it,

```c
int main(int argc, char** argv) {
    if (x > 0) {
        return 1;
    }
    int x = 3;
    return 0;
}
```

```bash
$ cc test.c
test.c: In function ‘main’:
test.c:2:9: error: ‘x’ undeclared (first use in this function)
     if (x == 2) {
         ^
test.c:2:9: note: each undeclared identifier is reported only once for each
            function it appears in
```

The compiler doesn't know what `x` is until _after_ the `if` block, so we can't
use it yet.

You will get a similar message if you use a function before you define it.

```c
int main(int argc, char** argv) {
    int x = f(argc);
    return x;
}

int f(int z) {
    return z + 1;
}
```

```bash
$ cc test.c
test.c: In function ‘main’:

test.c:2:13: warning: implicit declaration of function ‘f’
             [-Wimplicit-function-declaration]
     int x = f(argc);
             ^
```

Two interesting things to note. First, this is only a warning, not an error.
Warnings in C approximately mean that you are relying on non-standard behavior.
It isn't technically an error, but it is generally unwise to rely on this
working exactly as you would expect.

C warnings are often very helpful, so I would recommend enabling all of them
when you are compiling, like this,

```bash
$ cc -Wall test.c
```

Second, note that the warning says `implicit declaration` instead of
"undeclared". The compiler doesn't know what the function signature (i.e. the
return type and type of the arguments) so it uses some default signature. If
that default happens to be exactly what we wanted, the program will work. If it
is not, then our program will have issues. It is much safer to fix this program
instead of hoping that the default signature matches what we want.

```c
int f(int z) {
    return z + 1;
}

int main(int argc, char** argv) {
    int x = f(argc);
    return x;
}
```

## Duplicates and Shadows

You can also have issues if you declare the same variable twice,

```c
int main(int argc, char** argv) {
    int x = 1;
    int x = 2;
    return x;
```

```bash
$ cc -Wall test.c

test.c: In function ‘main’:
test.c:3:9: error: redefinition of ‘x’
     int x = 5;
         ^
test.c:2:9: note: previous definition of ‘x’ was here
     int x = 4;
         ^
```

If we want a new variable, we should declare a variable with a name that is
different than `x`. If we want to simply update the value of `x`, then we need
to remove the `int`.

```c
int main(int argc, char** argv) {
    int x = 1;
    x = 3
    int y = 2;
    return x;
```

Perhaps surprisingly though, this works,

```c
int main(int argc, char** argv) {
    int x = 1;
    if (argc > 1) {
        int x = 10;
    }
    return x;
}
```

What value will this program return? If we compile with `-Wall`, we'll get a
hint,

```c
$ cc -Wall test.c
test.c: In function ‘main’:
test.c:4:13: warning: unused variable ‘x’ [-Wunused-variable]
         int x = 10;
             ^
```

This tells us that the `x` defined in the `if` block is never used. Which means
we must be returning the first `x`. So the program returns `1`.

The general rule is that a variable comes "into" scope when it is declared, and
goes "out of" scope when execution reaches the end of the block that the
variable was defined in.

Blocks are most often sections of code that start with `{` and end with `}`. So
each function is a block, and each `if` or `while` statement also has a block.

Stepping through our example,

```c
int main(int argc, char** argv) {
```

The function signature introduces two variables into scope: `argc` and `argv`.

```c
int x = 1;
```

Declaring `x` brings it into scope.

```c
if (argc > 1) {
```

We enter the `if` block.

```c
int x = 10;
```

We declare a new variable `x`, that "shadows" the existing `x`. This is slightly
risky because we now have two variables with the same name. Any access to `x`
inside this `if` block will be referencing the new one, not the original one.

```c
}
```

We exit the `if` block, and the second `x` goes out of scope. Now we only have
the original `x`

```c
return x;
```

We return `x`, which is the original `x = 1`.

```c
}
```

We reach the end of `main`, and `x`, `argc`, and `argv` go out of scope.

# Functions

The last rule to clarify is that functions start from a fresh scope. So if we
have this program,

```c
int main(int argc, char** argv) {
    int x = 1;
    x = x * 2;
    return x;
}
```

And decide to move `x * 2` to its own separate function like this,

```c
int timesTwo() {
    return x * 2;
}

int main(int argc, char** argv) {
    int x = 1;
    x = timesTwo();
    return x;
}
```

We'll get an error.

```bash
$ cc -Wall test.c
test.c: In function ‘timesTwo’:
test.c:2:12: error: ‘x’ undeclared (first use in this function)
     return x * 2;
            ^
```

If you want a function to have access to a variable that you are working with,
you need to pass it as an argument,

```c
int timesTwo(int a) {
    return a * 2;
}

int main(int argc, char** argv) {
    int x = 1;
    x = timesTwo(x);
    return x;
}
```

## Naming Variables

Picking good names for variables can be difficult. You are free to name them
whatever you want, but I would recommend you make them descriptive and avoid
shadowing if possible. Repeating variable names can have surprising
consequences. Compare these two programs,

```c
int main(int argc, char** argv) {
    int x = 0;
    while (x < 10) {
        x = x + 1;
    }
    return x;
}
```

```c
int main(int argc, char** argv) {
    int x = 0;
    while (x < 10) {
        int x = x + 1;
    }
    return x;
}
```

One of these works correctly, but the other loops infinitely.
