---
title: "Memory"
subFiles: []
---

Every process on your computer requires memory. Your web browser, music player,
and text editor all need to store some data in memory while they are running.
But there is only a finite amount of memory available, so it must be managed.

The operating system is responsible for managing access to and ownership of
memory. Your computer probably has a few billion bytes of memory in RAM, but
let's pretend it only has 8 bytes.

When your computer starts up, the operating system needs to keep some of that
memory for itself.

```
+=======+=======+=======+=======+=======+=======+=======+=======+
| (OS)  | (OS)  |       |       |       |       |       |       |
+=======+=======+=======+=======+=======+=======+=======+=======+
^       ^       ^       ^       ^       ^       ^       ^
0       1       2       3       4       5       6       7
```

Here I am listing the owner of the byte inside the box. So the operating system
owns the first two bytes (0 and 1).

Now, if a process `A` starts up, it can request memory from the operating
system. Suppose it needs 4 bytes to run correctly. It sends a request to the
operating system for 4 bytes of memory, and the operating system will send back
a block of 4 bytes.

```
+=======+=======+=======+=======+=======+=======+=======+=======+
| (OS)  | (OS)  |       |  (A)  |  (A)  |  (A)  |  (A)  |       |
+=======+=======+=======+=======+=======+=======+=======+=======+
^       ^       ^       ^       ^       ^       ^       ^
0       1       2       3       4       5       6       7
```

Process `A` can now store any data it wants in bytes 3 - 6. However, it cannot
access bytes 0, 1, 2, or 7. If it tries to read or write a byte it does not own, the
operating system will block the access and crash the program.

This is an important feature. Imagine if you were typing in your text editor,
and it caused your music player to skip to the next song. Or, even worse, if you
were typing in a password in your browser and a different process stole the
password. Maintaining isolation between different sections of memory is critical
for your computer to operate securely and reliably.

Now imagine process `B` starts up and requests 4 bytes of memory from the
operating system. Unfortunately, it doesn't have 4 bytes to give. The OS needs 2
bytes for itself and `A` is already using 4. So the OS responds to `B`'s request
for 4 bytes with a failure, and `B` is responsible for deciding what to do next.

`B` might be able to get by with just 2 bytes, so it might send another request
to the operating system for 2 bytes instead of 4. At this point, there's a
problem. The operating system _has_ 2 bytes that it can give to `B`, but they
aren't next to each other.

This problem is called "fragmentation". There are multiple fragments, or small
chunks, of memory, that could be combined into larger chunks if they were moved
around.

To solve the problem, the operating system might shuffle memory around to look
like this,

```
+=======+=======+=======+=======+=======+=======+=======+=======+
| (OS)  | (OS)  |  (A)  |  (A)  |  (A)  |  (A)  |       |       |
+=======+=======+=======+=======+=======+=======+=======+=======+
^       ^       ^       ^       ^       ^       ^       ^
0       1       2       3       4       5       6       7
```

and then give bytes 6 and 7 to `B`.

It is easy to see that moving `A`'s memory will open up enough space for `B` to
get the 2 bytes it needs, but actually performing that change is difficult. The
operating system needs to move `A`'s memory _without telling `A`_.

Memory management is a difficult task, but the operating system mostly handles
it for you. The main responsibility you have as a programmer is to tell the
operating system when you are done with the memory so that it can be used by
other processes.
