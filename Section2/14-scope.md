---
title: Scope
...

# Scope

Our next task is to see how the stack and heap affect you when programming. The
easiest way to see this is with functions. Here is an attempt at writing a
function that increases the value of the argument by one:

```c
#include <stdio.h>

void plus_one(int x);

int main(int argc, char **argv) {
    int c = 10;

    plus_one(c);

    printf("The value of c is: %d\n", c);

    if (c == 11) {
        printf("Changed!\n");
        return 0;
    } else {
        printf("Not changed!\n");
        return 1;
    }
}

void plus_one(int x) {
    x += 1;
}
```

I've added a bit of error handling to our code. This is an easy way to test what
we believe is true. That way, it is easier to find bugs quickly. Let's run this
and see what happens:

```bash
$ gcc -Wall plus_one.c -o plus_one
$ ./plus_one
The value of c is: 10
Not changed!
```

First, I added the `-o` flag to our compiler call. This chooses an **o**utput
name for our compiled program other than `a.out`. It is an easy quality of life
change. Now we can keep different versions in the same directory. Or we can just
know by name what the executable file should do.

Second, our program failed. We hoped to add one to `c`, but we did not. This is
because we are not actually being given `c`, we are given it's value. Here is
the sequence of events that the stack processes:

```
Right before we call plus_one(c)

       +------+
       | argc |  \
       +------+  |
       | argv |  | Belongs to main
       +------+  |
       |   c  |  /
       +------+
```

```
We call plus_one(c)

       +------+
       | argc |  \
       +------+  |
       | argv |  | Belongs to main
       +------+  |
       | c=10 |  /
       +------+
       | x=10 |  Belongs to plus_one
       +------+
```

```
x += 1
       +------+
       | argc |  \
       +------+  |
       | argv |  | Belongs to main
       +------+  |
       | c=10 |  /
       +------+
       | x=11 |  Belongs to plus_one
       +------+
```

```
leave plus_one(c);

       +------+
       | argc |  \
       +------+  |
       | argv |  | Belongs to main
       +------+  |
       | c=10 |  /
       +------+
       |      |  plus_one's stack is cleared
       +------+
```

So we have found our culprit! C passes arguments to functions "by value", which
means the function is given the value of the variable, not the variable itself.
That's why our `x` was made. It holds the value of `c` when the function starts.
But when we change it, we aren't affecting `c` at all.

To fix this, we have two options. The first is to `return` the new value, and
update `c` to match. That would look like this, with the old version in comments
above the changes:

```c
#include <stdio.h>

// void plus_one(int x);
int plus_one(int x);

int main(int argc, char **argv) {
    int c = 10;

    // plus_one(c);
    c = plus_one(c);

    printf("The value of c is: %d\n", c);

    if (c == 11) {
        printf("Changed!\n");
        return 0;
    } else {
        printf("Not changed!\n");
        return 1;
    }
}

// void plus_one(int x);
int plus_one(int x) {
    x += 1;

    // no old code
    return x;
}
```

Running this proves that it works, but let's convince ourselves that it
**should** work! Here are the stack changes:

```
We call plus_one(c)

       +------+
       | argc |  \
       +------+  |
       | argv |  | Belongs to main
       +------+  |
       | c=10 |  /
       +------+
       | x=10 |  Belongs to plus_one
       +------+
```

```
x += 1
       +------+
       | argc |  \
       +------+  |
       | argv |  | Belongs to main
       +------+  |
       | c=10 |  /
       +------+
       | x=11 |  Belongs to plus_one
       +------+
```

```
c = 11; // plus_one(c); is replaced with its return value

       +------+
       | argc |  \
       +------+  |
       | argv |  | Belongs to main
       +------+  |
       | c=11 |  /
       +------+
       |      |  plus_one's stack is cleared
       +------+
```

The keyword `return` effectively sets the value of calling the function to the
return value. In our case, `plus_one(c) == 11`. The function is run, and then
the return value is substituted into its place. So we set `c == plus_one(c) ==
11`. It is slightly confusing to set `c` to a value that includes it, but we
have seen something like it before. `x = x + 1` first evaluates `x + 1`, and
then stores that result back into `x`. This works the same way.

So our first option was to explicity set the value of `c` to the returned value.
But we have another tool that can work here: pointers! Here's the pointer
version with the original and `return` versions in comments:

```c
#include <stdio.h>

// void plus_one(int x);
// int plus_one(int x);
void plus_one(int *x);

int main(int argc, char **argv) {
    int c = 10;

    // plus_one(c);
    // c = plus_one(c);
    plus_one(&c);

    printf("The value of c is: %d\n", c);

    if (c == 11) {
        printf("Changed!\n");
        return 0;
    } else {
        printf("Not changed!\n");
        return 1;
    }
}

// void plus_one(int x) {
// int plus_one(int x) {
void plus_one(int *x) {
    *x += 1;

    // no original code
    // return x;
}
```

Well that looks an awful lot like the original. All we had to add was 3 `*`'s
and 1 `&`. Two of the `*`'s we have seen before. `int *x` means that `x` holds
the address of an `int`.

`&` is how we take the address of a value. I like to think "ampersand"/"and"
*sortof* sounds like "address", so that's how I remember. So `&c` gives us a
pointer to `c`.

`*` is the opposite. It is how we "dereference" a pointer. A pointer is a
"reference" to a value, so "de"-"referencing" it gives us back the value.
So `*x += 1` increases the value at address `x` by 1. One final sequence of
stacks:

```
We call plus_one(c)

       +------+
       | argc |  \
       +------+  |
       | argv |  | Belongs to main
       +------+  |
    |->| c=10 |  /
    |  +------+
    |--| x=3  |  Belongs to plus_one
       +------+
```

```
*x += 1
       +------+
       | argc |  \
       +------+  |
       | argv |  | Belongs to main
       +------+  |
    |->| c=11 |  /
    |  +------+
    |--| x=3  |  Belongs to plus_one
       +------+
```

```
plus_one finishes

       +------+
       | argc |  \
       +------+  |
       | argv |  | Belongs to main
       +------+  |
       | c=11 |  /
       +------+
       |      |  plus_one's stack is cleared
       +------+
```

Either of the two styles we used is a good solution. It is good to understand
the tradeoffs though. The `return` version uses some extra space because it has
to copy `c`. This isn't a problem with single `int`'s, but it could start
causing problems if you are passing bigger data around. It is also very clear
that `plus_one` is changing the value of `c`.

In contrast, the pointer version is quite efficient. The only space we need is
to store the pointer. However, it becomes much harder to tell if `plus_one`
modifies `c`. From the function declaration, we know that it uses the address of
`c` somehow. But we have to look at the actual function body to know what
happens.

These tradeoffs represent two different ends of the spectrum of program design.
On the one hand, you worry about space efficieny. Does your program use a
reasonable amount of memory when it runs? It's always good to keep that number
as low as possible. However, you also have to consider further work on the
program. Have you documented and designed your program well enough that a reader
should know what `plus_one` does to `c`? If not, it may be worth it to use the
`return` version. How easy it is to read and understand your program is a good
measure of its quality.

This post has grown quite lengthy due to the pictures, but I haven't even made
it to the heap yet! Next time, we will solve the problem of passing new data
back up to `main`.

[Variable Scope 2](15-scope2.html)
