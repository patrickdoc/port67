---
title: "Memory in C"
subFiles: []
---

C adds another layer on top of the memory system, so let's review what we have
seen so far.

Inside your computer, there is a physical component that provides random access
memory. While the computer is turned on, the CPU can write and read data into
into the provided memory for temporary storage.

The operating system is responsible for managing access to this memory. When we
need to access extra memory in our program, we can request that the OS
"allocate", or make us the owner of, a new chunk of memory. When we are done
using that memory, we can release it back to the OS so other processes can use
it.

The OS also provides other services for security and efficiency that we might
accidentally run into. If we try to read or write memory that we don't have
access to, the OS will prevent that from happening and crash our program to
prevent any issues.

C builds on this by breaking your program's memory into three separate pieces:
stack, heap, and static memory.

## The Stack

So far, we have only interacted with the "stack". The stack is C's automatic
memory management. All of our programs so far have relied entirely on C to
manage the memory for us. Our variables are created on the stack and then
destroyed when they are no longer needed.

The stack goes hand in hand with our scope rules.

When we declare a variable, it comes into scope and C allocates memory on the
stack for it.

When execution reaches the end of the block, the variable goes out of scope and
C releases the memory it was using.

Here is a program that fails to compile,

```c
int main(int argc, char** argv) {
    if (argc == 1) {
        int value = 1;
    } else {
        int value = 2 * argc;
    }
    return value;
}
```

```
test.c:7:12: error: ‘value’ undeclared (first use in this function)
     return value;
```

Even though we declare `value` and assign it an `int`, when execution reaches
`return` that variable doesn't exist anymore. Not only do the scope rules say
that we can no longer access `value`, but C has released the memory associated
with `value`, so we couldn't access it even if we wanted to.

## The Heap

Allocating memory on the stack requires you to know ahead of time how much
memory you will need. That is not always easy though. Imagine you are writing a
very simple chat app. So simple in fact, that users are only allowed to post one
`char` at a time. You might start with something like this,

```c
int main(int argc, char** argv) {
    char messages[?];
    ...
}

The question is what number to put where the `?` is. You could put a small
number like 10, but then your app would only accept 10 messages and then stop
working.

You could also put a huge number like 10 billion. But that isn't very satisfying
either. If your app is not popular and doesn't have many messages on it, the
program will hold onto much more memory than it needs. Alternatively, if your
app becomes extremely popular, your app might use up all 10 billion messages and
then stop working.

The heap is a separate area of memory that lets us request memory only when we
need it. Now your program might look like this,

```c
#include <stdlib.h>

int main(int argc, char** argv) {
    int messageCount = 1;

    char* messages = malloc(sizeof *messages * messagesCount);
    if (messages == NULL) {
        return 0;
    }

    messages[0] = 'a';

    while (1) {
        // Add space for a new message
        messages = realloc(messages, sizeof *messages * (messageCount + 1));
        if (messages == NULL) {
            return 0;
        }

        // Get message from user and store it in messages array
        messages[messageCount] = 'a';

        messageCount = messageCount + 1;
        if (messageCount > 100) {
            break;
        }
    }

    // Important!
    free(messages);

    return messageCount;
}
```

This program uses memory much more efficiently. We don't guess how much memory
we will need. Instead, we ask for it when we need it.

There are a few new things in this program, so let's step through them.

```c
#include <stdlib.h>
```

This tells C that we are going to use some of the tools that already exist. In
particular, the three new functions: `malloc`, `realloc`, and `free`.

```c
char* messages = malloc(sizeof *messages * messagesCount);
```

We start with the familiar expression `char* messages`, which declares
`messages` to be a pointer to a `char`. In C, an array of `char` and a pointer
to a `char` are the same thing. Remember that accessing an element of an array,
like `messages[3]`, is the same as starting at the address of the first element
(`messages[0]`) and jumping forward 3 spaces.

Next is the new function `malloc`. To use `malloc`, you input the number of
bytes you would like, and the system returns a pointer to a block of memory that
size.

Instead of manually providing the number of bytes you want, it is usually best
to use this equation to let C figure out exactly how many you need. Here is the
equation with parentheses added for visual clarity,

```c
sizeof(*variable) * length
```

`sizeof` is a built-in keyword in C that returns the number of bytes that a type
requires. Sometimes types can be different sizes depending on what machine you
are using. For example, `int` can require either 4 or 8 bytes depending on the
way your computer is configured. So it is best to let C handle that for you. The
other parameter is just how many of those you want. In the example above, we
start by just requesting 1 `char` worth of memory.

```c
if (messages == NULL) {
    return 0;
}
```

`malloc` can fail if you request more memory than is available on your computer
at that time. If it does fail, it will return the special `NULL` pointer. You
should always check for and handle failures to avoid crashes and unintended
behavior.

```c
messages = realloc(messages, sizeof *messages * (messageCount + 1));
```

This is similar to the original `malloc` function, but here we are asking C to
make our existing block of memory larger. The main difference with `realloc` is
that we pass the existing pointer as the first argument. The second argument
works the same way. We are asking for the new block to have space for one more
`char` than it had before.

```c
if (messages == NULL) {
    return 0;
}
```

Again, we check for failure.

```c
free(messages);
```

This is the final new function. `free` tells C that we are done with the memory
and it can be released back to the computer.

This is both a blessing and a curse. By manually managing the memory, you can
use it much more efficiently, requesting and releasing it exactly when you need
it. But you can also use it much more inefficiently if you forget to call
`free`. Or you can introduce bugs by `free`ing a block of memory too early.

# Static Memory

The final type of memory in C is static memory. This data is loaded when the
program is started and deleted when the program is finished. This is mostly the
data that you put directly in the program source.

```c
int counter;

int incrementCounter() {
    counter = counter + 1;
    return counter;
}

int main(int argc, char** argv) {
    int i = 0;
    while (i < argc) {
        incrementCounter();
        incrementCounter();
        i = i + 1;
    }
    return counter;
}
```

```bash
$ ./a.out alice bob
$ echo $?
6
```

The variable `counter` is accessible anywhere in your program. In some sense,
this is like the scope rules. In this case the "block" that `counter` is
defined in is the entire program. It is available as soon as you declare it and
is available for the rest of the program.

# Conclusion

Just like writing books, there are lots of different ways to write programs. As
you gain experience writing programs, you'll develop habits and preferences that
you might call your "style". There are lots of different styles out there, so
it's good to keep an eye out for new techniques you haven't seen before.

You can write programs that use any and all of these types of memory. They each
come with their own pros and cons though, so as you practice with them try to
keep track of the different problems that they either cause or solve.
