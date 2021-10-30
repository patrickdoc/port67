---
title: "C Functions"
subFiles: []
---

One last tool allows us to reuse code and provide structure to our program. We
can create functions that allow us to call the same exact code without
copy-pasting it multiple times.

Let's write a program that returns `argc`, except when `argc` is 2 or 3. When
`argc` is 2 or 3, it should return 2^2 (2 times 2) and 3^3 (3 times 3 times 3). We might
write the program like this,

```c
int main(int argc, char** argv) {

    if (argc == 2) {
        int result = 2;
        while (argc > 1) {
            result = result * 2;
            argc = argc - 1;
        }
        return result;
    } else if (argc == 3) {
        int result = 3;

        while (argc > 1) {
            result = result * 3;
            argc = argc - 1;
        }
        return result;
    } else {
        return argc;
    }
}
```

In general, programmers don't like repeating code. Partly because we like being
lazy, but also because it is too easy to create problems by copy-pasting
incorrectly. We can avoid copy-pasting by creating a new function. Given an
integer x, it should return x^x.

We know what the function should do, so we can write the "signature" that
describes the function,

```c
int power(int x) {
}
```

We don't have the body of the function yet, but what we have looks similar to
`main`. Our function returns an `int`, it is named `power`, and it requires an
`int` as input. We can use `x` inside the body of the function just like we use
`argc`. So our function might look like this,

```c
int power(int x) {
    int result = x;
    int counter = x;

    while (counter > 1) {
        result = result * x;
        counter = counter - 1;
    }

    return result;
}
```

You should be able to match up the variables in the `power` function with the
variables that we used in `main`. The biggest difference is that here we are
using `x` instead of the plain `2` or `3` that we used above.

To use our new function, we "call" it in the body of `main`.

```c
int power(int x) {
    int result = x;
    int counter = x;

    while (counter > 1) {
        result = result * x;
        counter = counter - 1;
    }

    return result;
}

int main(int argc, char** argv) {
    if (argc == 2) {
        int value = power(2);
        return value;
    } else if (argc == 3) {
        int value = power(3);
        return value;
    } else {
        return argc;
    }
}
```

Now, when the program reaches `power(2)`, it jumps up into the `power` function,
and sets `x` equal to 2. It runs the body of the function, calculates that 2^2
is 4, and then returns 4. When it returns, the program jumps back to where it
was in the `main` function. `power(2)` is evaluated as 4, so `value` is set
to 4.

To save a bit of space, we can just return the result of the function
directly. `return power(2)` is the same as `return 4`.

```c
int main(int argc, char** argv) {
    if (argc == 2) {
        return power(2);
    } else if (argc == 3) {
        return power(3);
    } else {
        return argc;
    }
}
```

Verifying that it works,

```bash
$ ./a.out
$ echo $?
1
$ ./a.out arg1
$ echo $?
4
$ ./a.out arg1 arg2
$ echo $?
27
$ ./a.out arg1 arg2 arg3
$ echo $?
4
```
