---
title: main Breakdown
section: Section2
prev: 2-control-flow.html
next: 4-variables.html
...

# Main Breakdown

When writing Python, we could place code outside of any function, or at the
top-level, and the interpreter would execute it.  This program prints `1` every
time you run it.

```python
x = 1
print x
```

Writing a Python file and passing it to the program `python` is exactly the
same as opening the REPL and typing the code line by line. So the above
program is equivalent to,

```python
>>> x = 1
>>> print x
```

C, on the other hand, does not work like this. In C, there is *always* a
`main` function that looks like this,

```c
int main(int argc, char **argv) {
    // Code goes here
}
```

When the program is compiled and run, it calls the `main` function. This
requirement makes it harder to play around like we can in Python, and is part
of the reason why C doesn't have an interpreter. Just like in Python though, we
can define other functions and import code from other files so our programs have
some structure.

If we are going to have to write `main` the same way every time, it is probably
a good idea to get a sense of what it is. Let's build up piece-by-piece.

## Return Type

```c
int
```

The first thing we declare for any function is the return type. That is, the
type of the value that the function will return. In this case, `main` always
returns a single integer representing the success or failure of a program. It
is standard to return 0 for success, and 1 for failure. You can also return
other numbers to represent particular failures.

As an example, your program might take two numbers and divide them. If the
code successfully runs, it returns 0. If the user tries to divide by 0, you
could return 1. If the user gives a letter instead of a number, then you
might return 2. When you give your code to other people, it is best to document
the various return values, so they know what went wrong.

## Function Name

```c
int main
```

Next comes the name of the function. `main` is special, in that it is the
single function that C requires to be there. You can name other functions
anything you want, but it is standard to use lowercase and underscores to
separate words. For example: `multiply`, `test_code`, `blargh`. Names typically
should be as short and descriptive as possible.

## Function Arguments

```c
int main(int argc
```

The comma separated values inside the parentheses are the arguments to the
function, just like in Python. The only difference is that we include types
here. The first argument to `main` is an integer (**arg c**ount) indicating the
number of arguments passed on the command line. We will test this out in a bit.

```c
int main(int argc, char **argv) {
    // Code here
}
```

The next argument (**arg v**ector) is the list of arguments passed on the
command line. We are going to need to do a bit more work to understand exactly
what the `char **` type means, but I will show you what it represents in a
moment.

## Function Body

After that, we just have the `{}`'s which mark the start and end of the body
of code and the comment which starts with `//`. I want to remind you that we
could write this function like this,

```c
int main(int argc, char **argv) {//Code here}
```

But we always want to strive to make code as readable as possible. One good way
to ensure code is easy on the eyes is to keep it consistent. And so, we will
stick with,

```c
int main(int argc, char **argv) {
    // Code here
}
```

## Importing

We just need one more piece to demonstrate `main`, and that is to import our
printing function, `printf`. Instead of Python's `import`, we use `#include`
and surround the name with `<>`'s.

```c
#include <stdio.h>
```

Without further ado, here is a program that demonstrates the arguments to
`main`. There are things that I haven't talked about yet, but hopefully you
can get the jist.

```c
#include <stdio.h>

int main(int argc, char **argv) {

    printf("There are %d arguments.\n", argc);
    for (int i = 0; i < argc; i++) {
        printf("Argument %d: %s\n", i, argv[i]);
    }
    return 0;
}
```

Compiling and running,

```bash
$ gcc demo_main.c
$ ./a.out
There are 1 arguments.
Argument 0: ./a.out
$ ./a.out this is a test
There are 5 arguments.
Argument 0: ./a.out
Argument 1: this
Argument 2: is
Argument 3: a
Argument 4: test
```

You should try getting rid of that horrible grammar mistake `There are 1
arguments`. All it takes is an `if` statement, which you know all about!
