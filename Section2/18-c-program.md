---
title: C Program
...

# A Program in C

I've presented an enormous amount of information in this section without
providing any real examples tying it all together. Here is my attempt at that.

## The Map

Our program is going to be a small robot game. We are going to start with a map,
or grid. This will represent the ground that the robot has to walk over. Here is
what one might look like,

```
      0   1   2   3   4
    +---+---+---+---+---+
  0 |   |   |   |   |   |
    +---+---+---+---+---+
  1 |   |   |   |   |   |
    +---+---+---+---+---+
  2 |   |   |   |   |   |
    +---+---+---+---+---+
  3 |   |   |   |   |   |
    +---+---+---+---+---+
  4 |   |   |   |   |   |
    +---+---+---+---+---+
```

I've also added a coordinate system to help us talk about individual squares.
We read the position of a square as `(row, column)`.  The top left corner is the
square `(0,0)`. The top right is `(4,0)`. The bottom left is `(0,4)`. And the
bottom right is `(4,4)`.

A plain map is going to be pretty boring, so let's add some stuff inside. For
the sake of simplicity, let's just have empty squares, breakable rocks, and
walls. This is going to turn into a sort of maze that we will guide our robot
through.

Because we have three types of terrain, we need three representatives. Let's say
that `0` means empty, `1` means rock, and `2` means wall. We can then fill up
our map like so,

```
      0   1   2   3   4
    +---+---+---+---+---+
  0 | 0 | 2 | 0 | 1 | 1 |
    +---+---+---+---+---+
  1 | 0 | 2 | 0 | 1 | 0 |
    +---+---+---+---+---+
  2 | 1 | 2 | 0 | 0 | 0 |
    +---+---+---+---+---+
  3 | 0 | 0 | 0 | 0 | 2 |
    +---+---+---+---+---+
  4 | 2 | 2 | 2 | 0 | 1 |
    +---+---+---+---+---+
```

For a little practice reading our map, we can say that there is a clump of three
wall squares at `(0,1)`, `(1,1)`, and `(2,1)`.  There is also another wall group
at the bottom in squares `(4,0)`, `(4,1)`, and `(4,2)`.  Our robot will start at
`(0,0)` and try to get to `(4,4)` with our help.

### The Data Structure

We need to choose a way to store the values in our grid. We would also like a
good way of accessing individual cells. We've actually seen a good way to do
this already, but let's work through it anyway because it is probably not
obvious.

Looking at a single row,

```
    +---+---+---+---+---+
    | 0 | 2 | 0 | 1 | 1 |
    +---+---+---+---+---+
      0   1   2   3   4
```

a structure should jump out at us. This looks an awful lot like an array. In
fact, if we have a pointer to the beginning of the array called `row`, then we
can get the second to last element with `row[3]`. So we have a good way to
represent a single row.

But what about the rest of the grid? We need to store multiple rows. We have
already used a variable that stores multiple arrays though, `argv`. `argv`
stores multiple arrays of `char`'s. We need to store multiple arrays of `int`'s,
so we can steal the same solution.

We can create a variable called grid that is of type `int grid[5][5]`. In
pieces, we can think of this type as `int grid[5]`, which is an array of 5
`int`'s and then `int grid[5][5]`, which is an array of 5 arrays of 5 ints. If
we have 5 arrays and each holds 5 ints, then we can hold 25 ints.

To check that this variable is what we think it is, let's do a sanity check.

```
#include <stdio.h>

int main(int argc, char **argv) {
    int grid[5][5];

    printf("size of grid is: %d\n", sizeof(grid));

    return 0;
}
```

When run, we'll see that our grid is 100 bytes. `25 int * 4 bytes per int = 100
bytes`. This definitely covers what we need to store the grid, what about
access?

Using our array notation, we can access the square `(1,3)` like so:
`grid[1][3]`. This is about as good as we could hope for. The translation is
almost identical, and we don't even have to change any numbers. In words, this
access says "give me the fourth element of the second array." So we have both
the storage and access properties that we need.

### The Implementation

Let's set up the initial code for our map.

```c
#include <stdio.h>

int main(int argc, char **argv) {

    // Our map
    int grid[5][5] = {
        { 0, 2, 0, 1, 1},
        { 0, 2, 0, 1, 0},
        { 1, 2, 0, 0, 0},
        { 0, 0, 0, 0, 2},
        { 2, 2, 2, 0, 1}
    };

    // Check that it worked
    // Should print 0 1 2 1
    printf("(0,0): %d, (0,4): %d, (4,0): %d, (4,4): %d\n", grid[0][0]
                                                         , grid[0][4]
                                                         , grid[4][0]
                                                         , grid[4][4]);

    return 0;
}
```

We specifically set up our map to work well with arrays. If we wanted to make
`(0,0)` the bottom left, we would have needed to do some work to initialize the
map. But because we followed the order of arrays, we can just do it like this.

If you remember, we can set the initial values of arrays with the `{}` syntax.

```c
int arr[3] = { 2, 25, 23 };
```

We are using the same idea, just doing multiple arrays at once.

```c
int arr[3][2] = { {1, 2}, {3, 4}, {5, 6} };
```

This example uses 3 arrays with 2 elements each. Our grid code uses 5 arrays
with 5 elements each.

And that's all we need for now. We have a basic map and can query the various
cells to see what is in them. Next time, we will add a robot.

[A C Program 2](19-c-program2.html)
