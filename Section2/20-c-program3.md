---
title: C Program 3
...

# Accepting Commands

The last major component of our game will be an interface for accepting
commands. We are going to read them from a file, but you should be able to
adjust the code to work from the command line fairly easily.

We can start with our snippet from the IO lesson,

```c
// Create a variable that will represent our file
FILE *file;

file = fopen("file.txt", "r");

if (file == NULL) {
    printf("Can't open specified file\n");
    exit(1);
}

do_something(file);

fclose(file);
```

Instead of `"file.txt"` though, we can accept a file on the command line. Here
is the code with that change.

```c
// Make sure a file was passed as an argument
if (argc != 2) {
    fprintf(stderr, "Expected a single file argument\n");
    exit(1);
}

// Create a variable that will represent our file
FILE *file;

file = fopen(argv[1], "r");

if (file == NULL) {
    printf("Can't open specified file\n");
    exit(1);
}

do_something(file);

fclose(file);
```

All we had to do was replace the file name and add a check to make sure a file
name was given.

We can now rename the `do_something()` function to `read_commands()`, and write
the command interface for our game. We need some sort of specification for
what the commands in the file should look like. Let's limit our initial commands
to,

    right
    left
    up
    down

Each will be on its own line to make it easier for us to "parse", or read and
interpret, the file. Assuming we have such a file, we can write
`read_commands()`.

```c
#include <string.h>

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
```

I used a function called `strcmp` here, for "string comparison." If two strings
are equal, then it returns 0. The line read from the file includes the `\n`
character, so make sure the string you compare it to also
has the newline.

We can now plug in our movement function(s).

[A C Program 4](21-c-program4.html)
