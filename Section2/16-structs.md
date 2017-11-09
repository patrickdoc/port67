---
title: Structs
section: Section2
prev: 15-scope2.html
next: 17-io.html
description: Learn about data structures in C
...

# Data Structures

One of the main jobs of programs and programming languages is to represent data.
We've worked with many small representations through types. Each type tells us
how to interpret a few bytes as numbers or letters or strings. But it's time to
start thinking bigger. We want to be able to work with data that represents
books, people, cars, cities, planes, or anything you can think of.

As we talked about before, modern computers, phones, and even coffee-makers
handle enormous numbers of bytes regularly. It would be pretty difficult to make
all that work correctly if all we had were `int`'s and `char`'s. So we need a
new way to add structure to our data that gives it some extra meaning.

In C, the `struct` keyword lets us create "data structures", types composed of
smaller types that represent something new. We have almost all of the ideas that
we need to understand structs, so let's look at an example.

Many old arcade games had a high score list so that you could have a score to
aim for. Getting your name on the top 5 or 10 list would mean that everyone who
played the game could see how good you are. We will be storing and working with
these high scores, so let's create a new `struct` that holds them.

```c
struct HighScore {
    char initials[4]; // 3 initial ID, like AAA
    int score;        // The high score
};
```

This is a simple struct, but it helps us with a couple things. First, we can
return this struct from functions. If we didn't have this, we wouldn't be able
to return both the initials and the score. You can only return one value in C.
But with our `HighScore` struct, we can return both at the same time.

Second, we can make our code clearer. Consider two declarations of the same
function:

```c
char *better_score(int score1, char *inits1, int score2, char *inits2);
char *better_score(HighScore hs1, HighScore hs2);
```

Reading these declarations, what would you guess these functions do? The first
one is a little confusing, but there are scores and initials that go together
somehow. The second one is not perfect, but it is a little better. We know that
this function takes two `HighScore` structs, and returns some string. If we look
at the `HighScore` struct, we might guess that this function returns the
initials of the better score.

This is not a rule that structs are always better, but it does give some support
to that idea. One final consideration is if we wanted to add another field to
the struct. Imagine we wanted to add a time, then our functions would look like,

```c
char *better_score(int score1, char *inits1, int time1,
                   int score2, char *inits2, int time2);
char *better_score(HighScore hs1, HighScore hs2);
```

As you can see, it fills up space quickly.

## Using Structs

Enough talk, let's use them,

```c
#include <stdio.h>

struct HighScore {
    char initials[4];
    int score;
};

int main(int argc, char **argv) {

    // Declare hs as a variable of type `struct HighScore`
    struct HighScore hs;

    // Initialize the initials field of hs
    hs.initials[0] = 'A';
    hs.initials[1] = 'A';
    hs.initials[2] = 'A';
    hs.initials[3] = '\0';

    // Initialize the score field of hs
    hs.score = 0;

    return 0;
}
```

First, we declare our variable `hs`. It has type `struct HighScore`. We need to
tell C that `HighScore` is a struct, so it can find it. Next, we access the
fields of the struct with `.`. The syntax is `variable_name.field_name`. Here we
set the value of the field, but we can also access the value the same way,

```c
#include <stdio.h>

struct HighScore {
    char initials[4];
    int score;
};

int main(int argc, char **argv) {

    // Declare hs as a variable of type `struct HighScore`
    struct HighScore hs;

    // Initialize the initials field of hs
    hs.initials[0] = 'A';
    hs.initials[1] = 'A';
    hs.initials[2] = 'A';
    hs.initials[3] = '\0';

    // Initialize the score field of hs
    hs.score = 0;

    printf("initials are: %s\n", hs.initials);
    printf("score is: %d\n", hs.score);

    return 0;
}
```

## Struct Internals

`hs` is just like any other variable. We can pass it to functions, we can get a
pointer to it, and we can change it's value. But we don't know how the type is
stored in memory or how it is interpreted. Luckily, we have all the pieces to
understand it, so lets take a look.

                   struct HighScore
           +=====+=====+=====+=====+=====+
           | int |char |char |char |char |
           +=====+=====+=====+=====+=====+
    bytes:    4     1     1     1     1    =     8

Our struct contains 8 bytes of data. If we ask C how big it is with,

```c
printf("Size of HighScore is: %zu\n", sizeof(HighScore));
```

We will see that C thinks it is 8 bytes too. What if our initials only had two
characters though? We can check this by just changing the field declaration,

```c
char initials[3];
```

Now if we run exactly the same code, we get output of... 8. But why? Our struct
should look like this in memory,

               struct HighScore
           +=====+=====+=====+=====+
           | int |char |char |char |
           +=====+=====+=====+=====+
    bytes:    4     1     1     1    =   7

There are only seven bytes of data, so why does our struct use eight? There is
an idea called "alignment" that is causing this. It is easiest for the computer
if it can access multi-byte values (like `int`'s) at multiples of their size.
Since our `int` is four bytes, it should start at address 0,4,8,12,16 ... and so
on. If we had a pair of our structs in an array, they would line up like this,

            struct HighScore        struct HighScore
        +=====+=====+=====+=====+=====+=====+=====+=====+
        | int |char |char |char | int |char |char |char |
        +=====+=====+=====+=====+=====+=====+=====+=====+
    bytes: 4     1     1     1     4     1     1     1
    addr:  0     4     5     6     7     11    12    13

Unfortunately, 7 is not a multiple of 4, so this won't work. We have to "pad"
the struct to fill out to a multiple of 8. In this case, we just have to include
and extra byte of all 0's. With the padding, our pair looks like this.

               struct HighScore       |      struct HighScore       |
        +=====+=====+=====+=====+=====+=====+=====+=====+=====+=====+
        | int |char |char |char |  0  | int |char |char |char |  0  |
        +=====+=====+=====+=====+=====+=====+=====+=====+=====+=====+
    bytes: 4     1     1     1     1     4     1     1     1     1
    addr:  0     4     5     6     7     8     12    13    14    15

Now things line up properly, but we do waste 2 bytes. Ordering structures to
pack them as tightly as possible is usually not very important, but it is good
to know. For example, look at what happens with this small change,

```c
struct HighScore {
    char first;  // First initial
    int score;   // the high score
    char middle; // The middle initial
    char last;   // The final initial
}
```

How many bytes will this struct require? We still only have 7 bytes of data. But
since we have to line up the `int` after we already put one `char`, this
structure takes up twelve bytes.

                               struct HighScore
           +=====+=====+=====+=====+=====+=====+=====+=====+=====+
           |char |  0  |  0  |  0  | int |char |char |  0  |  0  |
           +=====+=====+=====+=====+=====+=====+=====+=====+=====+
    bytes:    1     1     1     1     4     1     1     1     1   =  12
    total:    0     1     2     3     4     8     9     10    11

Just by moving one little `char`, we now have five wasted bytes instead of 1. If
you start working with big data, this can add up!

We can also now understand how the `.` operator works. Just like arrays, it
works with offsets from the start. The only difference is that arrays can do `x
* sizeof(type)` while structs have to match up the field names with their proper
offset. For this struct,

```c
struct HighScore {
    int score;
    char initials[];
}
```

We can calculate the offsets at compile time.

    hs.score
    hs + offset(score)
    hs + 0
    hs

and for the initials

    hs.initials
    hs + offset(initials)
    hs + 4

Our `.` is just simple addition to find the field in memory.
