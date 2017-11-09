---
title: Interpreted vs. Compiled
section: Section2
next: 2-control-flow.html
description: Learn about compiling C
...

# Compiling

I briefly displayed a C "Hello, world!" program for you before, but here it is
again as a refresher.

```c
#include <stdio.h>

int main(int argc, char **argv) {

    printf("Hello, world!");
    return 0;
}
```

As always, I suggest you make a directory (`~/Projects/c`) and a file to work in
(`hello_world.c`). If this were python, we could then run our program like this:

```bash
$ c hello_world.c
```

Unfortunately, C is a compiled language, so we have to do a little more work.
We have used the program `python` to read code written in the language Python
and run it. When writing C, or any other compiled language, you need to use a
program called a "compiler". Most systems will have either `gcc` or `clang`.
If you run `gcc --version` on a Mac, you might find that even though you asked
for `gcc` you are actually getting `clang`.

To compile C using these programs, you can simply pass the file as an argument.
I'm going to use `gcc` in my examples, but just substitute `clang` if your
system only has `clang`.

```bash
$ gcc hello_world.c
```

If we now view the contents of our directory with `ls`, we will find a new file
called `a.out`. This is called an "executable" file. That is, we can "execute"
or "run" it using `./a.out`. If you type just `a.out` at the command line, you
will get a warning like,

```bash
-bash: a.out: command not found
```

This is because `bash` has a list of places that it looks for executables, and
`a.out` isn't in any of those places. But you can tell `bash` where it is using
`.`. This is just shorthand for "right here". You can use any valid path for
`a.out` to tell `bash` how to find it. For example, adjusting these to be
appropriate for your computer, any of these should work:

```bash
$ ./a.out
$ ../c/a.out         # Assuming you followed my structure above
$ ~/Projects/c/a.out # Assuming you followed my structure above
$ /Users/USERNAME/Projects/c/a.out # On Macs
$ /home/USERNAME/Projects/c/a.out  # On Linux
```

The last two use "absolute" paths, which you can find using `pwd` in the
directory that holds `a.out`.

## Special Characters

If you did run any of those commands above, you might find something strange.
The output did not work as well as hoped,

```bash
$ ./a.out
Hello, world!$ ../c/a.out
Hello, world!$
```

When we call `print` in Python, it automatically sticks a newline on the end of
what we ask it to print out. As will come up again and again with C, it does
practically nothing that you don't explicitly tell it to do. To fix our
problem, we just need to add a newline character to our string.

```c
#include <stdio.h>

int main(int argc, char **argv) {

    printf("Hello, world!\n");
    return 0;
}
```

`\n` is a special character. The `\` indicates that the next character should
be treated specially. This helps us in a couple tricky situations. The first is
the newline. If you were to include a new line by using the enter/return key,
your program's formatting would get messed up. So we just use `\n`. Sometimes
you might want to print the `"` character in your string. If you left it bare,
C would assume it was the end of the string you wanted to print, and the rest
would just confuse it. With `\"` though, it is clear what you mean. Just note
that this also means you have to use `\\` to print `\`.

As an example, we can print a variety of things.

```c
printf("I heard that \\ is called a \"backslash\"\n");
```

Putting that in place of our "Hello, world!", we can see it prints reasonably.
However, it does sacrifice readability. Navigating these issues is part of
language design. Many times you will come across code that looks like `\"\n"`
and it will seem completely absurd to you. Figuring out how the authors got
there is often the fastest way to learning.

One way to look at this is to try to think up a way to clearly indicate quotes
to be printed and quotes that indicate the start/end of a string. `\"` is a
reasonably compact representation of the difference. One of the more elegant
solutions is even simpler though: use `'`. We can get rid of two `\`'s just
by using single quotes.

```c
printf("I heard that \\ is called a 'backslash'\n");
```

`\n` is just the tip of the iceberg of places where we have to be careful in
C. However, I mentioned before that C offers us power in exchange for our time.
`\n` is useful, so useful that Python includes it. Try this in the Python
interpreter.

```python
>>> print "line 1\nline 2"
line 1
line 2
>>>
```

The design choice that Python made was to simply assume that you *always*
want a newline at the end of the strings you print. Which is a reasonably fair
assumption, but does hide things from us. C assumes nothing, which is both a
blessing and a curse.
