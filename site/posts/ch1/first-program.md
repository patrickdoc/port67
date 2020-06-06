---
title: "Your First Program"
---

We are now ready to move on to the second piece of your computer: the central
processing unit, or CPU.

The CPU is responsible for executing all of the programs running on your
computer. Some of the instructions in the programs are for the CPU itself, for
example, math operations, like adding, multiplying, and comparing numbers. Other
instructions must be coordinated with other components.  When you write to a
file, the filesystem sends a request to the CPU, and then the CPU sends a
request to the hard drive and waits for it to finish.

As we saw last time, all of these instructions live in files. But instead of
being formatted for humans to read, they are formatted for the computer.[^1] In
fact, the format they use changes depending on the specific configurations of
your computer.

[^1]: On Linux, you may have noticed that the executables all have `ELF` at the
  beginning. That isn't just coincidence, it stands for "Executable and Linkable
  Format" and tells the computer what kind of file it is.

Rather than learn the specifics of every computer configuration, we are going to
learn a programming language. Programming languages are generic ways of writing
instructions for the computer. Once we write a program in a given language, we
use a compiler to translate from the generic programming language into
configuration-specific instructions.

We are going to use the C programming language. It is old and a little bit less
user-friendly than some of the newer ones. But, it doesn't hide any of the
details from us, which makes it useful for learning about what is happening
inside your computer.

## A Simple Program

Writing your first program is remarkably easy once you have the tools. You only
need two: Vim and `cc`, short for "C compiler".

First, we need to write the program. (This is the last time I'll include
commands for setting up your directories. Adding it here for one last
refresher.)

```bash
$ cd
$ cd projects
$ mkdir C
$ cd C
$ vim test.c
```

Inside `test.c` we are going to write the simplest C program we can. Replace
`55` below with your favorite number.

```c
int main(int argc, char** argv) {
    return 55;
}
```

While it is not immediately clear how this program works, it is at least a bit
more readable than `ls` was. We'll come back to what everything means here, but,
for now, let's compile your first program.

```bash
$ cc test.c
$ ls
a.out   test.c
```

`cc` generated the executable `a.out` from our program. If you open `a.out` in
Vim, you should see something similar to what you saw in the `ls` executable.

Now we can try to run our executable. When you run commands like `ls` and `pwd`,
the terminal will look through a specific set of directories to find the
corresponding executables. [1] (This set of directories is called your "PATH"
and can be displayed with the command `echo $PATH`) Our `a.out` executable is
not in one of those directories, so we need to give the terminal a little extra
help to find it. Instead of using `a.out`, we'll use `./a.out`. `.` means "the
current directory", so `./a.out` means "the file named `a.out` in this
directory".

```bash
$ ./a.out
```

Our program is not very impressive. There are no obvious signs that it did
anything. One way that we can prove to ourselves that it ran though is to check
the "exit code". Whenever a program runs in the terminal, it finishes by
`return`ing a number that is typically used to indicate if the program was
successful or not. In our case, that number will be whatever you replaced `55`
with in the program above.

```bash
$ ./a.out
$ echo $?
55
```

The command `echo $?` displays the exit code of the previous command.

Feel free to experiment by changing the number in the program, recompiling,
rerunning, and displaying the exit code.

```bash
$ vim test.c
$ cc test.c
$ ./a.out
$ echo $?
```

## Assembly

The compiler does not actually translate directly from the programming language
to instructions for the computer. There is an intermediate stage called
"Assembly", which is the closest we can get to pure instructions for the
computer.

We can use the `-S` flag to ask `cc` to stop after translating from C to
Assembly so that we can look at it.

```bash
$ cc -S test.c
$ ls
a.out  test.c  test.s
```

Now we can open our Assembly file `test.s`, and we see something like this,

```as
    .file    "test.c"
    .text
    .globl   main
    .type    main, @function
main:
.LFB0:
    .cfi_startproc
    pushq   %rbp
    .cfi_def_cfa_offset 16
    .cfi_offset 6, -16
    movq    %rsp, %rbp
    .cfi_def_cfa_register 6
    movl    %edi, -4(%rbp)
    movq    %rsi, -16(%rbp)
    movl    $55, %eax
    popq    %rbp
    .cfi_def_cfa 7, 8
    ret
    .cfi_endproc
.LFE0:
    .size    main, .-main
    .ident   "GCC: (Debian 8.3.0-6) 8.3.0"
    .section    .note.GNU-stack,"",@progbits
```

This is where the instructions become computer specific, so your results may
look different from mine. If we trim it down to the bare minimum it should be
closer though.

```as
main:
    pushq   %rbp
    movq    %rsp, %rbp
    movl    %edi, -4(%rbp)
    movq    %rsi, -16(%rbp)
    movl    $55, %eax
    popq    %rbp
    ret
```

These are the types of instructions that run your entire computer. They are
all very small and mostly just about shuffling numbers around. Our `return 55;`
becomes two simple instructions.

First, `movl    $55, %eax`. This instruction tells the CPU to move the number
`55` to the special location named `eax`. If you replaced `55` with your
favorite number, you would see that number here instead.

Second, `ret`. This signals that the program is done.

It is a common convention to put the return code in the `eax` location. So when
a program like the terminal wants to check the return code of `a.out`, it knows
to look at `eax`.

Assembly does not come up often in modern programming because it is tedious to
work with and we have better tools available to us. But it's important to at
least have a sense of how it works.

For example, one surprising fact about the CPU is that it only works with
numbers.

As an experiment, edit your `test.c` file to return `'A'` instead of your
favorite number. Then pause and make a guess as to what you will see if you
recreate the Assembly or compile and run your program.

Once you have your guess and have changed your file, either regenerate the
assembly,

```bash
$ cc -S test.c
$ vim test.s
```

or recompile and run to see the result.

```bash
$ cc test.c
$ ./a.out
$ echo $?
65
```

You probably did not guess that `65` would be the result. You might ask, why 65?
Why not 64, or 66, or 1542? The answer is mostly because someone decided that's
how it would be many years ago. But to understand why we have to convert `'A'`
to a letter at all, we need to cover our final topic.
