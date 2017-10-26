---
title: The Heap
...

# Scope 2: The Heap

Our problems last time were focused around modifying somethin that already
existed. Our function `plus_one` was attempting to update the value of `c`,
which was already in memory. We didn't have to worry about `main`'s stack
getting cleared, like we did with `plus_one`.

But what if we wanted to create a completely new value from within a function,
and then use it again after that function's stack has been cleared? We know of
one way to return new values from our function: `return`! That might look
something like:

```c
#include <stdio.h>

int new_value();

int main(int argc, char **argv) {
    int c = new_value();

    printf("The value of c is: %d\n");

    return 0;
}

int new_value() {
    return 42;
}
```

But with a function as simple as this, we might as well just use `int c = 42;`.
What if we want to return an array of ints? Unfortunately, C won't let us return
an actual array. But we already know how to get around that, we'll just use
pointers!

```c
#include <stdio.h>

int *new_value();

int main(int argc, char **argv) {
    int *c = new_value();

    printf("c[0] = %d\n", c[0]);
    printf("c[1] = %d\n", c[1]);
    printf("c[2] = %d\n", c[2]);

    return 0;
}

int *new_value() {
    int arr[3] = {1, 2, 3};
    return arr;
}
```

If we run this code, we will get:

```bash
$ gcc -Wall string.c
string.c:17:12: warning: function returns address of local variable [-Wreturn-local-addr]
    return arr;
           ^~~
$ ./a.out
Segmentation fault
```

The compiler is pretty sure that we are doing something wrong, so it warns us.
Sure enough, when we run the code we get the dreaded "Segmentation fault". A
segmentation fault occurs when something illegal happens in memory. In our case,
it happens when we try to access the value at `c[0]`. This bug is tricky to
track down. Without the warning, it would be hard to determine what was causing
the error. You could try deleting the access to `c[0]`, but then the other two
would cause the same problem. So you have to delete all of them to get the code
to run.

But, we are going to apply our knowledge to see why this is an error. The answer
is the location of `arr` in memory. It is on the stack!

    +------+
    | argc |  \
    +------+  |
    | argv |  | Belongs to main
    +------+  |
    |  c   |  /
    +------+
    |  1   |  \
    +------+  |
    |  2   |  | arr in new_value's stack
    +------+  |
    |  3   |  /
    +------+

So we set `c`'s value to be a pointer to `arr`, and then immediately clear all
the data there! And we are left with this:

      +------+
      | argc |  \
      +------+  |
      | argv |  | Belongs to main
      +------+  |
    |-|  c   |  /
    | +------+
    |> ?????

But there is nothing there. And so, C informs us that there has been a memory
error.

## Malloc

The time has come to use the heap. We want `c` to be a valid pointer after the
function returns. That means we need memory outside of the stack. To get it, we
call `malloc()`, for **m**emory **alloc**ation.

```c
#include <stdio.h>
#include <stdlib.h>

int *new_value();

int main(int argc, char **argv) {
    int *c = new_value();

    printf("c[0] = %d\n", c[0]);
    printf("c[1] = %d\n", c[1]);
    printf("c[2] = %d\n", c[2]);

    return 0;
}

int *new_value() {
    int *arr = malloc(3 * sizeof(int));

    if (arr == NULL) {
        printf("WARNING: malloc failed!\n");
    }

    arr[0] = 1;
    arr[1] = 2;
    arr[2] = 3;

    return arr;
}
```

We have four new ideas to break down from that one piece of code. First,
`malloc` takes an integer representing the number of bytes you want to store. We
would like to store 3 `int`'s, so we ask for 3 times the number of bytes needed
to store a single `int`. The argument to `malloc` is very commonly `x *
sizeof(thing)` where `x` is the number of `thing`'s you want to store.

Second, we need to check if the value returned by `malloc` is `NULL`. It is
possible that `malloc` fails to find memory for you to use. In that case, it
will return `NULL` instead of an address to the heap. If you then try to use
`NULL`, you will likely get a "segmentation fault", often shortened to
"segfault". In this code, we just loudly print that it failed. Normally you
should try to do some error handling so that you don't use a bad pointer.

Third, we don't get to initialize the array like we could before. You need to
explicitly set all of the values that you want to be in the array. Using any of
the addresses in the array before you set them is undefined behavior. `arr[0]`
could be 0, or it could be -678912346, there is no way to know.

Finally, we need to `#include <stdlib.h>` to use `malloc`. Just like we use
`#include <stdio.h>` for `printf`, we need to tell C where it can find the
function.

## Allocating Strings

One last example to help solidify our knowledge of a couple things. Let's return
a new string from our function:

```c
#include <stdio.h>
#include <stdlib.h>

char *new_value();

int main(int argc, char **argv) {
    char *c = new_value();

    printf("Our string is: %s\n", c);

    return 0;
}

char *new_value() {
    char *arr = malloc(4 * sizeof(char));

    if (arr == NULL) {
        printf("WARNING: malloc failed!\n");
    }

    arr[0] = 'H';
    arr[1] = 'e';
    arr[2] = 'y';
    arr[3] = '\0';

    return arr;
}
```

After changing all the `int *`'s to `char *`'s, there is only one big
difference. We have to remember the null byte on the end of our string! If we
want a string that has 3 characters, like `"Hey"`, we need to ask for **4**
bytes of memory so that we can make sure the last byte is null.

The really big problem here is that if you forget, you aren't always punished.
It could be that the next byte in memory happens to be 0, in which case things
will operate as expected. But if that changes for any reason, suddenly your
program will stop working. C strings always end with a null byte! Don't forget
it!

# Next
[Structs](16-structs.html)
