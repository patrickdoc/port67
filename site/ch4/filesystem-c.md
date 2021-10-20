---
title: "Filesystem Programming"
subFiles: []
---

There is a command line program named `cat`, that prints the contents of a file
to `stdout`. If we create a basic skeleton program named `test.c`, then `cat`
might look like this,

```bash
$ cat test.c
int main(int argc, char **argv) {
    return 0;
}
---

We are going to write our own version of `cat` to practice working with
`stdio`.

If you look at the `man` page for `stdio`, it tells you that you need to add
`#include <stdio.h>` to your program to work with these functions. We also need
the user to provide a file name on the command line, so we'll check that there
are enough arguments.

```c
#include <stdio.h>

int main(int argc, char **argv) {
    if (argc < 2) {
        return 1;
    }
    return 0;
}
```

Just like we had a pair of `malloc` and `free` for creating and destroying
memory, we have a pair of `fopen` and `fclose` for creating and destroying
streams. We can get the function signatures from the `man` pages.

```bash
$ man fopen
SYNOPSIS
       #include <stdio.h>

       FILE *fopen(const char *pathname, const char *mode);

RETURN VALUE
       Upon  successful  completion fopen(), fdopen() and freopen() return a FILE pointer.  Otherwise, NULL is returned and errno is set
       to indicate the error.

$ man fclose
SYNOPSIS
       #include <stdio.h>

       int fclose(FILE *stream);
```

The `mode` argument in `fopen` describes how we are going to use the stream. The
options are described in the `man` page for `fopen`, but we'll be using `"r"`
because we are only interested in reading the source file.

Adding the open and close to our program we now have,

```c
#include <stdio.h>

int main(int argc, char **argv) {
    if (argc < 2) {
        return 1;
    }

    FILE *source = fopen(argv[1], "r");
    if (source == NULL) {
        return 1;
    }

    fclose(source);
    return 0;
}
```

The value returned from `fopen` is of type `FILE *`. This is the type of our
streams. Our program doesn't know what a `FILE` is, so we can't work with one
directly. But, that doesn't stop us from storing a pointer to a `FILE`. All of
the `stdio` functions accept a `FILE *`, which tells them which `FILE` we are
working with. Internally, they access the raw `FILE`, and then return a result
to us.

Just like with `malloc`, we need to verify that the call to `fopen` succeeded,
so we check if `FILE *` is null before continuing. If `fopen` succeeded, we need
to make sure we call `fclose()` on the `FILE` when we are done with it.

All that is left now is to read data from the source and write it to `stdout`.
For that, we need two new functions: `fgetc` and `fputc`.

```bash
$ man fgetc
SYNOPSIS
       #include <stdio.h>

       int fgetc(FILE *stream);

RETURN VALUE
       fgetc(), getc() and getchar() return the character read as an unsigned char cast to an int or EOF on end of file or error.

$ man fputc
SYNOPSIS
       #include <stdio.h>

       int fputc(int c, FILE *stream);

RETURN VALUE
       fputc(), putc() and putchar() return the character written as an unsigned char cast to an int or EOF on error.
```

These two functions are very similar, the only difference is that we have to
tell `fputc` the character we want written to the stream.

The core of our program will now be a simple loop:
- Call `int c = fgetc(source)` to read a character from the source stream
- Check if `c == EOF`
  - If yes, end loop
  - If no, call `fputc(c, stdout)`, loop

```c
#include <stdio.h>

int main(int argc, char **argv) {
    if (argc < 2) {
        return 1;
    }

    FILE *source = fopen(argv[1], "r");
    if (source == NULL) {
        return 1;
    }

    int c = fgetc(source);
    while (c != EOF) {
        fputc(c, stdout);
        c = fgetc(source);
    }

    fclose(source);
    return 0;
}
```

And that's it! Try it out a bit to see it in action.

```bash
$ ./a.out test.c
#include <stdio.h>
....
$ ./a.out
$ echo $?
1
```

## printf

Now that we can write to the terminal, we should make our errors more
informative. For that we'll need what might be the most useful function in
programming: `printf`.

```bash
$ man printf
SYNOPSIS
       #include <stdio.h>

       int printf(const char *format, ...);
       int fprintf(FILE *stream, const char *format, ...);
```

The basic usage of `fprintf` takes a stream and a string, and writes the string
to the stream. For example,

```c
    if (argc < 2) {
        fprintf(stderr, "Expected at least 2 arguments\n");
        return 1;
    }
```

```bash
$ ./a.out
Expected at least 2 arguments
$ ./a.out test.c
#include <stdio.h>
...
```

This is a significantly better experience for the user than having to check
`echo $?` every time. But we can do slightly better.

The second "f" in `fprintf` stands for "format". The string that we provide to
`fprintf` can include placeholders that will be filled by variables. These
placeholders look like `%` followed by a letter that indicates the type of the
variable. For example,
- `%d` for `int`s
- `%c` for `char`s
- `%s` for `char *`s, otherwise known as strings

We can update our `fprintf` call to include the number of arguments given,

```c
    if (argc < 2) {
        fprintf(stderr, "Expected at least 2 arguments, only %d given\n", argc);
        return 1;
    }
```

```bash
$ ./a.out
Expected at least 2 arguments, only 1 given
```

`printf` is the same as `fprintf`, but it does not accept a `FILE *` argument
because it always prints to `stdout`.

## perror

If you look at the "RETURN" section of the `man` page for `fopen`, it says,

```bash
RETURN VALUE
       Upon  successful  completion ....  Otherwise, NULL is returned and errno is set
       to indicate the error.
```

Many standard library functions record why they failed in a global variable
named `errno`. This variable is just a number though, so it is not immediately
useful to us. The function `perror` handles both converting that number into a
useful string and printing it.

```bash
DESCRIPTION
       The perror() function produces a message on standard error describing the last error encountered during a call to a system or li‐
       brary function.

       First (if s is not NULL and *s is not a null byte ('\0')), the argument string s is printed, followed by a  colon  and  a  blank.
       Then an error message corresponding to the current value of errno and a new-line.

       To be of most use, the argument string should include the name of the function that incurred the error.
```

This will help us detect any failures from `fopen`.

```c
    FILE *source = fopen(argv[1], "r");
    if (source == NULL) {
        perror(argv[1]);
        return 1;
    }
```

```bash
$ ./a.out fake-file.txt
fake-file.txt: No such file or directory
```

Interestingly, if we try the same thing with `cat`,

```bash
$ cat fake-file.txt
cat: fake-file.txt: No such file or directory
```

Which looks awfully similar.

## Going Further

We can make small changes to our program to make it do exciting things.

First, try changing the program to read from `stdin` and write to the file
provided on the command line. You'll need to change your call to `fopen` to
indicate that you are going to write to the file. To do that, replace `"r"` with
`"w"`. NOTE: this will erase any file that already exists with that name, so be
careful!

When you run your program, it will wait for input from you. After you type some
characters, you will need a way to indicate `EOF`, otherwise you
will be stuck in the loop forever. Ctrl-D (control + d key at the same time)
should cause your loop to complete successfully.

Second, try changing the program to mimic the `cp` or "copy" program. It accepts
exactly 2 arguments, a source file and a destination file. After the program
runs, the contents of the destination file should exactly match the source file.

Try to be diligent about reporting errors. It helps the user of the program see
how it is intended to be used, but more importantly it helps you find and fix
issues quickly.
