---
title: "Composite Types"
---

From our primitive types, we can build more complex types in memory. The first
type we'll look at is the "array". An array is a collection of values that have
the same type. For example, names are a collection of `char`s, so we can put
them in an array.

In C, we declare a new array like this,

```c
char name[] = {'A', 'l', 'i', 'c', 'e'};
```

This statement defines a new variable `name` that holds 5 `char`s.

We can read and write values in this array by accessing a specific "index" of
the array. The index is just the position of the element in the array, counting
from the beginning. Perhaps surprisingly, we start counting from 0 instead of 1.
So the first element of the array has index 0, the second has index 1, and so
on.

```c
name[0] = 'B';
return name[1];
```

This makes a little more sense if we look at the array in memory. In the
interest of saving space, I am going to compress the individual bits into a
single block like this,

```
+---+---+---+---+   +---+---+---+---+
| 0 | 1 | 0 | 0 |   | 0 | 0 | 0 | 1 |
+---+---+---+---+   +---+---+---+---+
        \                   /
         \                 /
              +=======+
              |  'A'  |
              +=======+
```

Just remember that each `char` is composed of 8 bits of memory, or a byte.

Our `name` array now looks like this in memory.

```
                  name
+=======+=======+=======+=======+=======+
|  'A'  |  'l'  |  'i'  |  'c'  |  'e'  |
+=======+=======+=======+=======+=======+
```

All 5 characters perfectly lined up next to each other.

Internally, memory is accessed using addresses. When you start writing more
complex programs, you need to work with these addresses directly. But we won't
be using them right now, we just need to know how they work to understand array
indices.

Here is our `name` array again, this time though, I've added fake address
information. Our variable starts at byte address 543 and extends through byte
547.

```
name
V
+=======+=======+=======+=======+=======+
|  'A'  |  'l'  |  'i'  |  'c'  |  'e'  |
+=======+=======+=======+=======+=======+
^       ^       ^       ^       ^
544     545     546     547     548
```

In general, the variables we declare are just names for addresses in memory.
When you declare a new `char` variable named `my_char`, C finds an unused byte
of memory and associates the address with `my_char`.

Now the index trick is pretty simple. `name` is the address of the first
character. To get the address of the second character, we just need to jump over
the first one. To get the address of the third character, we need to jump over
the first two, etc.

```
name            name[2]
V               V
+=======+=======+=======+=======+=======+
|  'A'  |  'l'  |  'i'  |  'c'  |  'e'  |
+=======+=======+=======+=======+=======+
^       ^       ^       ^       ^
544     545     546     547     548
```

We can calculate the exact address like this,

```
name[2] = name + 2*sizeof(char)
name[2] = 544 + 2*1
name[2] = 546
```

So the index is just a counter of how many values we need to skip over before we
reach the one we want. Which is why we start counting from 0. When we want the
first `char` in the array, we don't want to skip any characters.

One nice feature of this system is that it doesn't matter how big the elements
are. `char`s are only one byte, but other types are bigger. For example,
`int`s are typically four bytes so that we can work with larger numbers; one
byte can only hold values up to 256, but 4 bytes can hold up to 4.3 billion.

Even if we change our array from `char`s to `int`s, our picture doesn't change.
Here it is again, but with `int`s,

```
name            name[2]
V               V
+=======+=======+=======+=======+=======+
|  123  |  943  |   2   | 1402  |  11   |
+=======+=======+=======+=======+=======+
^       ^       ^       ^       ^
544     548     552     556     560
```

The addresses are now 4 apart because each `int` is 4 bytes (or 32 bits) long.
But that doesn't change our address calculation.

```
name[2] = name + 2*sizeof(int)
name[2] = 544 + 2*4
name[2] = 552
```

No matter what type of objects or how many objects we store in our array, C
always knows how to find them by jumping over the earlier ones. Because they are
all the same size, it is easy to find any element you ask for.

## Structs

Arrays work well when you want to store many elements that have the same type,
but sometimes we want to group values of different types together.

Many old arcade games had high-score screens at the end. When you scored high
enough to make it to the list, you could record your scores with 3 letters to
identify yourself. So you might see an entry in the list like `AAA - 523` or
`BOB - 1500`.

We want to group the letters and score together so that we always can access
them at the same time and don't mix up which letters go with which numbers.

To do this, we just need to tell C about this new type so that it can map it
into memory. We call this new type a "data structure" because it builds a new
type out of existing types. In C, we can define a new data structure with the
`struct` keyword.

```c
struct HighScore {
    char    id[3];
    int     score;
};
```

We now have a convenient way to work with individual high score entries. We can
read and write the values similar to other variables.

```c
struct HighScore score1;
score1.id[0] = 'B';
score1.id[1] = 'O';
score1.id[2] = 'B';
score1.score = 1500;
```

Here `score1` is a variable that contains two values: an array of three `char`s
named `id` and an `int` score.

We can define a second `HighScore` variable just like the first,

```c
struct HighScore score2;
score2.id[0] = 'A';
score2.id[1] = 'A';
score2.id[2] = 'A';
score2.score = 523;
```

`HighScore` works just like any other type. When we declare new variables with
the type `struct HighScore`, C has to set aside memory to hold the values. That
looks like this,

```
score1
id                      score
V                       V
+=======+=======+=======+=======+
|  'B'  |  'O'  |  'B'  | 1500  |
+=======+=======+=======+=======+
^       ^       ^       ^       ^
544     545     546     547     551
```

When we ask C for `score1.score`, it has to perform a similar address
calculation to the array calculation. We know that `score1` starts at address
544, so we just need to know how far to jump to find the start of the `score`
value.

Our struct is laid out in memory exactly as we defined it in the code. In this
case, it is three `char`s followed by an `int`. To find the `score` then, we
just need to jump over the three `char`s.

```
score1.score = score1 + offset(score)
score1.score = 544 + 3*sizeof(char)
score1.score = 544 + 3*1
score1.score = 547
```

If tomorrow we decide to make the `id` an array of `int`s instead of `char`s, we
don't need to worry about changing the rest of our code. `score1.score` will
still work exactly as we want it to because it knows it needs to skip three
`int`s instead of three `char`s.

##

With the ability to build types out of smaller types, we can work with any data
we want. If we can define all the pieces that make up the data we are working
with, C can map it into memory for us. Once the data is in memory, we can
analyze it, transform it, or send it over a network to another computer.
