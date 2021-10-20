---
title: "Filesystem"
subFiles: []
---

From here on out, our programs are going to get significantly more interesting.
Instead of just returning a number from our programs, we are going to interact
with other parts of your computer, starting with the filesystem.

So far we've worked with the filesystem through the command line, using commands
like `ls`, `cd`, and `vim`. We've also seen that these commands are just files
themselves, although they are full of binary data instead of letters, numbers,
and spaces.

To work with files from our programs, we are going to need a clearer picture of
C's model of files. Whenever we are looking for this information, one reliable
source of information is the `man` (short for manual) pages. These can be
accessed either with the `man` command or online. For example,

```bash
$ man malloc
NAME
       malloc, free, calloc, realloc - allocate and free dynamic memory

SYNOPSIS
       #include <stdlib.h>

       void *malloc(size_t size);
       void free(void *ptr);
       ...

DESCRIPTION
       The  malloc()  function  allocates  size bytes and returns a pointer to the allocated memory.  The memory is not initialized.  If
       size is 0, then malloc() returns either NULL, or a unique pointer value that can later be successfully passed to free().
```

You can scroll up and down to read the page from the command line with `k` and
`j` like in Vim. To get back to the command line, press `q`.

This page is a concise summary of all the memory topics we just covered. It
shows you how to include the code you need to run these functions; it shows you
the function signatures for how to use them; and it even explains the various
return codes for how to handle success and failure.

To work with the filesystem, we are going to use the functions in `stdio`, which
is short for "Standard Input and Output". Input is how you pass information into
a program, and output is how the program passes information back to you or the
computer. This information is primarily going to be held in files, although we
need to slightly extend our definition of files to cover some of the common ways
`stdio` is used.

Here's what the `man` pages say about `stdio`.

```bash
$ man stdio
DESCRIPTION
       The  standard I/O library provides a simple and efficient buffered stream I/O interface.  Input and output is mapped into logical
       data streams and the physical I/O characteristics are concealed.
```

There are a lot of unfamiliar topics there, but we are going to focus on the
second sentence,

```
Input and output is mapped into logical data streams and the physical I/O
characteristics are concealed.
```

A "logical data stream" is just a sequence of data that can be read one-by-one.
For a regular text file, the data stream would just be all the characters listed
out in order, including all the letters, spaces, and punctuation.

By thinking of files as "logical data streams" instead of regular files, we get
to play an interesting trick. Suppose I asked you to read a book to me. You
would start at the beginning, read chapter 1, chapter 2, etc, and then get to
the end and say "The End!" or something because the book was over. The end is
well defined, so when you reach the last page, we both know the book is over.

However, suppose I asked you to read a text conversation to me. You might read a
few texts back and forth, and then say "the other person is still typing". The
conversation isn't over, but you don't have more data yet. You can't predict
what the other person is going to say, so we both just have to wait around until
you get a response.

This is an example of the "physical I/O characteristics" that are concealed. The
data stream might be connected to a file stored on your hard drive; it might be
connected to the terminal waiting for you to type; or it might be connected to
something like your mouse!

No matter what the stream is connected to, we will interact with it the same
way. Similar to allocating memory, we are responsible for creating the stream,
and then destroying it when we are done. For memory, we `malloc`'d the memory
when we needed it and `free`'d it when we were done. For streams, we will "open"
the stream when we need it and "close" it when we are done.

If we have a stream opened for a regular file, we can can read, write, and edit
it just like we do in Vim. In fact, our stream has a "position indicator" very
similar to the cursor in Vim. If we want to write "hello" at the end of a
stream, we first need to move the position indicator to the end of the file. If
we then want to read the first word of the file, we need to move the position
indicator back to the beginning.

When you run a program, there are three streams automatically created for you.
Standard input, or `stdin`, is a source of input for your program. You can read
from `stdin` to get data from users or other programs. Standard output, or
`stdout`, is a destination for data that you want to send. When you run your
program from the command line, the data you send to `stdout` will be printed in
the terminal. Standard error, or `stderr`, is another destination that
you can send data to. It also will be displayed in the terminal, but it is
generally used to report issues instead of regular information.
