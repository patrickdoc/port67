---
title: "Intro to C"
subFiles: [basic-c.md, basic-c-2.md, basic-c-3.md, basic-c-4.md, basic-c-5.md]
---

C is a useful starting point for learning about programming. It tends to be very
explicit, which makes reading and writing C code straight-forward. You can
usually walk through the code line by line to understand what it is doing and
how it is doing it.

However, this explicit nature of C is a double-edged sword. It will do
precisely what you write, even if that isn't quite what you mean. That's not to
say that other languages prevent you from making mistakes, but they at least
help you with some of the details that you might get wrong.

Despite the fact that many modern languages handle some of the details for you,
investing time into learning these is still worthwhile. For better or worse,
many languages have built upon not only the tools that C provides, but also the
design decisions. You'll see echoes of C everywhere in the programming world.

Here's our starting point for learning C,

```c
int main(int argc, char** argv) {
    return 0;
}
```
