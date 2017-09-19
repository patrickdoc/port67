# Arrays

From our basic types of `char`, `unsigned`, `int`, and `float`, we can construct
more types. The first we have access to is the "array". In C, an "array" is a
group of values all lined up next to each other. Declaring a new array variable
is easy:

```
char my_array[3];
```

This line declares a new variable named `my_array`, which holds 3 `char`'s.
Arrays can hold many values, but they all have to be the same type. This is due
to the size issue that types solved in the first place. If I am going to have
multiple values, then I definitely need to know how much space to allocate for
each of them.

In the upcoming pictures, I am going to introduce this shorthand for all of our
sanity.

```
                    char
    +---+---+---+---+   +---+---+---+---+
    | 1 | 0 | 0 | 1 |   | 1 | 0 | 0 | 0 |
    +---+---+---+---+   +---+---+---+---+
            \                   /
             \                 /
                  +========+
                  |  char  |
                  +========+
```

I am going to compress a type's bit representation into this smaller box so that
I can include multiple next to each other in a reasonable amount of space.

Back to our array, what does it look like? Well, it is 3 `char`'s perfectly
lined up in memory.

```
                    my_array
          +========+========+========+
          |  char  |  char  |  char  |
          +========+========+========+
```

Because we know how big a `char` is, we know how big a `my_array` is: `3 *
sizeof(char)`. In this case, `my_array` is 3 bytes. We can even check this to
make sure I know what I am talking about.

```
int main(int argc, char **argv) {

    char my_array[3];

    printf("my_array is %d bytes long\n", sizeof(my_array));
}
```

The `sizeof` keyword in C takes a variable or type and returns the number of
bytes it takes up. Running our code, we see:

```
$ ./a.out
my_array is 3 bytes long
```

If you change `my_array` to be an array of `int`'s instead of `char`'s, how big
will it be? Guess, and then check for yourself by modifying the code above.
Next, try guessing how changing the number of elements stored will affect the
size. Check that too.

At this point, you have seen enough `main` functions to replicate them on your
own, so I am going to just give you the important lines and let you fill in the
rest. Hopefully you have plenty of examples from previous lessons to work from
now!

We can initialize the values of an array just like so:

```
unsigned my_array[3] = {74, 11, 9};
```

If we put more numbers inside the `{}`'s than we have asked for, we *might* get
a warning. If we put fewer than we ask for, and then try to use an uninitialized
value, we almost certainly won't. These are the dangers of C. My goal is to give
you enough knowledge to avoid these issues most of the time and debug when you
have to.

To access values, we use a very similar syntax:

```
unsigned first  = my_array[0];
unsigned second = my_array[1];
unsigned third  = my_array[2];
```

This can be a point of confusion, so let's take a second to understand it. When
we count the elements of an array, we start at 0. Here's the picture again:

```
          |         my_array         |
          +========+========+========+
          |  char  |  char  |  char  |
          +========+========+========+
          |  first | second | third  |
```

The value we give within the `[]`'s is an "index". It represents some element
within the array. `my_array` tells us where the start, and then the index tells
us how much to move over. Let's update our picture:

```
       my_array
          |
          V
          +========+========+========+
          |  char  |  char  |  char  |
          +========+========+========+
                            ^
                            |
                     my_array + 2*sizeof(char)
```

So our index tells the computer how much to move over. The nifty thing is that
this syntax works even if we change types, and therefore size of elements in the
array.

```
       my_array
          |
          V
          +========+========+========+
          |   int  |   int  |   int  |
          +========+========+========+
                            ^
                            |
                     my_array + 2*sizeof(int)
```

As you can see, our picture is exactly the same! Because the compiler knows the
type of the elements in the array, it can adjust however it needs to. This takes
a lot of work off the programmer. Instead of trying to figure out how many bytes
of an offset we need, we just write `my_array[2]` and get the third element
every time.

Hopefully the index numbers make some sense now. To get the first element of the
array, we start at the beginning and move over `0*sizeof(type)`, where `type` is
the type of elements in the array. To get the last value in the array, we start
at the beginning and move over `(length - 1)*sizeof(type)`, where `length` is
the number of elements in the array.
