---
title: "C Main"
subFiles: []
---

Here is the starting point for most of the C programs we are going to write,

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
The body contains the code that gets executed whenever the function is run.

In our case, the body of `main` is just `return 0`. `return` stops execution of
the body and sends the value out of the function. The value returned from `main`
is often called the "return code" or "exit code" of the program because it is
the final result of executing the program. Once a value is returned from `main`,
the program is complete.

Finally, we have the closing `}` to indicate the end of the `main` function.
