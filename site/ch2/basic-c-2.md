---
title: "C Control Flow"
subFiles: []
---

There are other tools built into C that let us affect the control flow. We'll
cover a few here and pick up the rest along the way.

We can change the value of a variable during our program. For example, we could
subtract 1 from `argc`,

```c
int main(int argc, char** argv) {
    argc = argc - 1;
    return argc;
```

When we run it, we see that the value returned is 1 less than before,

```bash
$ ./a.out
$ echo $?
0
$ ./a.out arg1
$ echo $?
1
```

This leads us to our next tool for affecting control flow, the `while` loop.
`while` works similarly to `if`, but checks the condition again after executing
the body. If it is still true, it runs the code again, checks the condition,
runs the code, etc.

If you run this program it will subtract 1 from argc until argc is 0, then it
will return argc.

```c
int main(int argc, char** argv) {
    while (argc > 0) {
        argc = argc - 1;
    }
    return argc;
}
```

Running it to verify it works,

```bash
$ ./a.out
$ echo $?
0
$ ./a.out arg1
$ echo $?
0
```

Let's write a simple program that will return 2 to the `argc` power. That is, 2
times 2 times 2... `argc` times.

```c
int main(int argc, char** argv) {
    int result = 2;

    while (argc > 1) {
        result = result * 2;
    }

    return result;
}
```

If we try this out without any arguments, it works,

```bash
$ ./a.out
$ echo $?
2
```

But if we add arguments, the program doesn't return,

```bash
$ ./a.out

```

There is a problem with our program! Type control and c at the same time to stop
the program (Ctrl-C).

If we step through the program, we can spot this issue. First we create a
variable called `result` that will hold our final result. Our goal is to
multiply `result` by 2, `argc` times.

Next we check `(argc > 1)`. Because we provided `arg1` on the command line,
`argc` is 2. 2 is greater than 1, so this evaluates to `true`. `while` works
like `if`, so we execute the code inside the body. `result` is multipled by 2 to
become 4, and now we go back to the start of the `while` loop.

So we check the condition. `argc` is still 2, 2 is still > 1, so we execute the
body, and check the condition...

We forgot to substract 1 from `argc`. So our comparison of `argc > 1` will
*always* be true. Our program will continue looping infinitely because we
haven't set up our loop correctly. That's why we needed to use Ctrl-C to stop
it.

To fix the program, we just need to modify the body of the `while` loop
slightly,

```c
int main(int argc, char** argv) {
    int result = 2;

    while (argc > 1) {
        result = result * 2;
        argc = argc - 1;
    }

    return result;
}
```

Testing our program with arguments, it works now,

```bash
$ ./a.out
$ echo $?
2
$ ./a.out arg1
$ echo $?
4
$ ./a.out arg1 arg2
$ echo $?
8
```

---

One last tool allows us to reuse code and provide structure to our program. We
can create functions that allow us to call the same exact code without
copy-pasting it multiple times.

Let's write a program that returns `argc`, except when `argc` is 2 or 3. When
`argc` is 2 or 3, it should return 2^2 (2 times 2) and 3^3. We might
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
