---
title: "C Control Flow"
subFiles: []
---

Our basic program is very boring. No matter what we do, it always returns 0. We
can make it slightly more dynamic by reacting to the number of arguments passed
in from the command line.

```c
int main(int argc, char** argv) {
    return argc;
}
```

Now, instead of always returning 0, the program returns the number of arguments
that we passed in.

```bash
$ ./a.out
$ echo $?
1
$ ./a.out arg1
$ echo $?
2
$ ./a.out arg1 arg2
$ echo $?
3
```

We can take it a step further by examining the value before we return it. C
provides us with tools to affect the "control flow" of the program. You can
imagine that there is a little arrow pointing at the current line of code that
is running. Generally, the arrow just moves down one line at a time. But, we can
cause the arrow to skip lines or move backwards depending on what we want the
program to do.

We can modify our program slightly to treat input different. If `argc` is 1, we
will return 100, otherwise we will return `argc`.

```c
int main(int argc, char** argv) {
    if (argc == 1) {
        return 100;
    } else {
        return argc;
    }
}
```

The program will run almost exactly as we specified it: if `argc` is equal to 1
return 100, otherwise return `argc`.

Stepping through it, we start with `(argc == 1)`. `==` compares the values on
the left and right side. If they are the same, it evaluates to `true`. If they
are not the same, it evaluates to `false`.

`if (true)` will execute the code between the next braces `{}`. `if (false)` will skip that code.

`else` does the opposite of `if`. When `if` runs, `else` does not run. When `if`
does not run, `else` runs.

When there are no arguments provided to the program, `argc` will be 1. So `argc
== 1` evaluates `true`, and we run the code inside the `{}` after `if` which
returns 100 and ends the program.

```bash
$ ./a.out
$ echo $?
100
```

When there are arguments provided the the program, `argc` will *not* be 1. The
code in `{}` after `if` will be skipped, and the code in the `{}` after
`else` will run.

```bash
$ ./a.out arg1
$ echo $?
2
$ ./a.out arg1 arg2
$ echo $?
3
```

---

We can change the value of a variable during our program. For example, we could
subtract 1 from `argc`,

```c
int main(int argc, char** argv) {
    argc = argc - 1;
    return argc;
}
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
body and check the condition...

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
