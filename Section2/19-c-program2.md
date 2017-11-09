---
title: C Program 2
section: Section2
prev: 18-c-program.html
next: 20-c-program3.html
description: Create a structure to hold our robot
...

# Adding a Robot

With the map done, we now need a character to wander around it.

## The Robot

We need a way to store information about our robot. The first piece we will
want is a position on the grid. As we saw last time, this can be stored with two
integers. Once we realize that, a struct will probably be our best bet. So we
can start sketching our robot like so,

```c
struct robot {
    // The grid position
    int x;
    int y;
}
```

Our initialization code is almost always the same, so we can start writing that
too.

```c
#include <stdio.h>
#include <stdlib.h>

// snip

struct robot *robo = malloc(sizeof(*robo));

if (robo == NULL) {
    fprintf(stderr, "malloc for robot failed\n");
    exit(1);
}

robo->x = 0;
robo->y = 0;
```

That's a nice base to work from. If we add fields to our struct, all we have to
do is initialize them. `malloc` will automatically get the right amount of
memory from the type and our error handling also won't need updating.

With that established, what else will our robot need to do? As the goal is to
move from `(0,0)` to `(4,4)`, we will need a way to move. We will also need a
way to break the rocks that stand in our way.

## Movement

We come to a purely design based question now. The only way to know the answer
is to have experience writing this kind of code or having incredible foresight.
As most people don't have either, we are going to approach this a couple
different ways.

The crux of our problem lies in the type of the function(s) we will use for
moving our robot. Our options are to create one single function that may grow
fairly large, or create multiple smaller functions and risk repeating our code.
The only way to see the difference is to actually write it and see.

Option #1: Single Function

```c
// Move a robot one unit
// Return 0 on successful move, 1 on error
int move(struct robot *robo, unsigned direction) {

    // 0 -> left
    if (direction == 0) {
        if (robo->x - 1 < 0) {
            fprintf("error: moved off map\n");
            return 1;
        }
        robo->x -= 1;
        return 0;

    // 1 -> right
    } else if (direction == 1) {
        if (robo->x + 1 > 4) {
            fprintf("error: moved off map\n");
            return 1;
        }
        robo->x += 1;
        return 0;

    // 2 -> down
    } else if (direction == 2) {
        if (robo->y - 1 < 0) {
            fprintf("error: moved off map\n");
            return 1;
        }
        robo->y -= 1;
        return 0;

    // 3 -> up
    } else if (direction == 3) {
        if (robo->y + 1 > 4) {
            fprintf("error: moved off map\n");
            return 1;
        }
        robo->y += 1;
        return 0;

    // Bad direction
    } else {
        fprintf("error: unknown movement direction\n");
        return 1;
    }

}
```

Option #2: Multiple Function

```c
int move_right(struct robot *robo) {

    // Check move, assume grid has width 5
    if (robo->x + 1 > 4) {
        fprintf("error: moved off map\n");
        return 1;
    }

    // Apply move
    robo->x += 1;
    return 0;
}

int move_left(struct robot *robo) {

    // Check move, assume grid has width 5
    if (robo->x - 1 < 0) {
        fprintf("error: moved off map\n");
        return 1;
    }

    // Apply move
    robo->x += 1;
    return 0;
}

int move_up(struct robot *robo) {

    // Check move, assume grid has height 5
    if (robo->y + 1 > 4) {
        fprintf("error: moved off map\n");
        return 1;
    }

    // Apply move
    robo->y += 1;
    return 0;
}

int move_down(struct robot *robo) {

    // Check move, assume grid has height 5
    if (robo->y - 1 < 0) {
        fprintf("error: moved off map\n");
        return 1;
    }

    // Apply move
    robo->y -= 1;
    return 0;
}
```

As written, these implementations are very similar. They are only a couple of
lines different in length. The actual logic is identical aside from the extra
direction processing in the single function. So the differences are not very
clear yet. We mostly only know that the single function is a little more
flexible. Any direction can be passed, so we don't have to be sure which
direction we are moving when we want to move. In contrast, the multiple
functions are more rigid. They can only move in a single direction, but they do
tell us which direction that is.

You can never really be sure which solution will be best until you try to use
and extend it. For example, if we want to add a check to prevent walking onto
stones or walls we have to add it in four separate places. This is a hint that
we haven't found the perfect way to write this yet.

Another way to test the implementation is to try and use it. If we know exactly
how it is used, then we might be able to simplify things. Next time, we are
going to write the code that needs to call our movement functions.
