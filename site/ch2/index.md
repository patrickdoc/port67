---
title: "Intro to C"
subFiles: [basic-c.md, basic-c-2.md, basic-c-3.md, basic-c-4.md]
---

C is a useful starting point for learning about programming. It tends to be very
explicit, which makes reading and writing C code straight-forward. You can
usually walk through the code line by line to understand what it is doing and
how it is doing it.

However, this explicit nature of C is a double-edged sword. It will do
precisely what you write, even if that isn't quite what you mean. That's not to
say that other languages prevent you from making mistakes, but C is not always
very helpful in telling you what is wrong.

I've found that investing time into some of C's concepts has paid off not only
in C but also in many other languages that have inherited features from C. If
you look closely at other languages, you'll find many echoes of the design
decisions made in C. It has withstood the test of time, so it clearly has done
at least some things very well.

We'll pick apart the most basic C program and then start bringing in all the
other pieces one by one. As a preview, here is our first C program,

```c
int main(int argc, char** argv) {
    return 0;
}
```
