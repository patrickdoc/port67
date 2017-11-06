---
title: C Program 4
section: Section2
prev: 20-c-program3.html
...

# The Final Product

All that's left for us to do is assemble all of the pieces. The way I have done
it here isn't the only way to do it, so feel free to reorganize as you'd like.

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct robot {
    // The grid position
    int x;
    int y;
}

// Initialization functions
struct robot *new_robot();

void read_commands(FILE *file);

// Movement functions
int move_left(struct robot *robo);
int move_right(struct robot *robo);
int move_up(struct robot *robo);
int move_down(struct robot *robo);

int main(int argc, char **argv) {

    // Setup map
    int grid[5][5] = {
        { 0, 2, 0, 1, 1},
        { 0, 2, 0, 1, 0},
        { 1, 2, 0, 0, 0},
        { 0, 0, 0, 0, 2},
        { 2, 2, 2, 0, 1}
    };

    // Get the FILE *
    if (argc != 2) {
        fprintf(stderr, "Expected a single file argument\n");
        exit(1);
    }
    
    FILE *file;
    
    file = fopen(argv[1], "r");
    
    if (file == NULL) {
        printf("Can't open specified file\n");
        exit(1);
    }
    
    // Run game
    do_something(file);
    
    // Cleanup resources
    fclose(file);

    return 0;
}

// Create a new `struct robot`
struct robot *new_robot() {

    // Setup robot
    struct robot *robo = malloc(sizeof(*robo));
    
    if (robo == NULL) {
        fprintf(stderr, "malloc for robot failed\n");
        exit(1);
    }
    
    robo->x = 0;
    robo->y = 0;

    return robo;
}

// Read and execute sequence of commands from a file
void read_commands(FILE *file) {
    // An array that will hold each command.
    // We know 8 is big enough to hold any command, the newline, and the null
    char buffer[8];

    // Loop over every line in the file
    // if `fgets` cannot fill the buffer, it returns NULL
    while (fgets(buffer, sizeof(buffer), file) != NULL) {
        if (strcmp(buffer, "right\n") == 0) {
            // Move right
        } else if (strcmp(buffer, "left\n") == 0) {
            // Move left
        } else if (strcmp(buffer, "up\n") == 0) {
            // Move up
        } else if (strcmp(buffer, "down\n") == 0) {
            // Move down
        } else {
            // Unknown input
            fprintf(stderr, "error: unknown command %s\n", buffer);
            exit(1);
        }
    }
}

// Move 1 unit to the right
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

// Move 1 unit to the left
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

// Move 1 unit upwards
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

// Move 1 unit downwards
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

commands.txt

    down
    down
    down
    right
    right
    right
    down
    right

```bash
$ gcc -Wall robot.c
$ ./a.out commands.txt
You made it!
```
