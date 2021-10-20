---
title: "C Main"
subFiles: []
---

Time to become familiar with C syntax. Syntax is the set of rules that define
how we write programs in a given language. Here is the starting point for most
of the C programs we are going to write,

```c
int main(int argc, char** argv) {
    return 0;
}
```

Stepping through this program, we start with `int` which indicates that we are
going to return an integer at the end of our program. When we compile and run
our program in the terminal, we can see the integer that was returned. Assuming
your C program is named `test.c`,

```bash
$ cc test.c
$ ./a.out
$ echo $?
0
```

Next, we have `main`. When you run a C program, it starts by finding the
function named `main` and then executing the code inside.

`main` traditionally has two "arguments" or "inputs". The first is an `int`
named `argc` for "argument count". The second is an array of `char*` named `argv` for
"argument vector". We'll come back to `char**` in a bit because we don't have to
tools to explain it yet. But, conceptually, `argc` and `argv` correspond to the
count and value of the arguments passed into the program from the command line.

```bash
$ ./a.out
# argc = 1
# argv[0] = ./a.out
```

```bash
$ ./a.out arg1 arg2
# argc = 3
# argv[0] = ./a.out
# argv[1] = arg1
# argv[2] = arg2
```

After the `argc` and `argv` arguments comes an opening brace `{`. Everything
from `{` to its matching close brace `}` is called the "body" of the function.
The body contains the code that gets executed when the program starts.

In our case, the body of `main` is just `return 0`. `return` stops execution of
the body and sends the value out of the function. The value returned from `main`
is often called the "return code" or "exit code" of the program because it is
the final result of executing the program. Once a value is returned from `main`,
the program is complete.

Finally, we have the closing `}` to indicate the end of the `main` function.

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
provides us with basic math tools which we can use to affect the "control flow"
of the program. You can imagine that there is a little arrow pointing at the
current line of code that is running. Generally, the arrow just moves down one
line at a time. But, we can cause the arrow to skip lines or move backwards
depending on what we want the program to do.

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
the left and right side. If they are the same, it return `true`. If they are not
the same, it returns `false`.

`if (true)` will execute the code between the next braces `{}`; `if (false)`
will skip that code. When there are no arguments provided to the program, `argc`
will be 1. So `argc == 1` returns `true`, and we run the `return 100` line which
returns the value 100 and ends the program.

`else` does the opposite of `if`. When `if` runs, `else` does not run. When `if`
does not run, `else` runs. When there are arguments provided the the program,
`argc` will *not* be 1. The code in the `{}` after `if` will be skipped, and the
code in the `{}` after `else` will run.

Testing our program to verify it works,

```bash
$ ./a.out
$ echo $?
100
$ ./a.out arg1
$ echo $?
2
$ ./a.out arg1 arg2
$ echo $?
3
```

As expected, with no additional arguments, the program returns 100. But if there
are additional arguments, it just prints `argc`.
