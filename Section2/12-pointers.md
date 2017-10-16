# Scope

We have almost covered everything we need to understand the declaration of the
`main` function. For reference:

```c
int main(int argc, char **argv) {}
```

The last piece to decipher is `char **argv`. As with other variable
declarations, `argv` is just the name of the variable. And we know from our
study of types that `char` is a one-byte value that is interpreted as an ASCII
character. So what's the deal with the `**`'s?

I briefly introduced `argc` and `argv` way back in the beginning. They both help
us process the arguments on the command line. If we compiled a program into a
file called `a.out`, and ran it like so:

```bash
$ ./a.out a b arg
```

`main` would be able to check the values of `argc` and `argv` to see that there
were 4 arguments `["./a.out", "a", "b", "c"]`. Thus `argc` counts the number
of arguments, and `argv` stores them all.

We know how to store a single string from our last lesson, it is a null
terminated array of `char`'s. We have also seen that we can store multiple
values of a type in arrays, so it might be our inclination to imagine `argv` as
an array of strings. Ignoring the first element of the array for the sake of
brevity, let's look at this hypothetical.

```
                                     argv
  +========+========+========+========+========+========+========+========+
  |   'a'  |  '\0'  |   'b'  |  '\0'  |   'a'  |   'r'  |   'g'  |  '\0'  |
  +========+========+========+========+========+========+========+========+
```

Remember that each `char` takes up exactly one byte. So our variable `argv` is 8
bytes. As `argv` is an array, it would be ideal if we could get the first
argument with `argv[0]` and the second with `argv[1]` and so on. However, how
would that work? As we talked about last time, strings can be any length! There
is no way for the compiled program to know how long the string arguments will
be. I could just have easily passed:

```bash
$ ./a.out aa bb arg3
```

And `argv` would have needed 11 bytes. So there is no way for our index trick to
work. If we had `argv[1]` and wanted to move over 1 "string" length, we could
not find the first string without just looking for the first `\0` byte in the
array. So this solution won't work if we want a reasonable way to access the
different arguements.

To solve this, we need to use memory more flexibly. So far we have been packing
our data in tightly. Values have been given an exact size, with a little bit of
wiggle room depending on the type. But with strings, there could really be any
number of characters. `char`'s fit nicely into a single byte. `int`'s at 2 or 4
bytes cover most use cases. But strings can easily be a single word or an entire
book, so we need a different way to store them.

The good news is that we don't need to be overly concerned with how well we are
utilizing the space we have. Remember that we have **billions** of bytes to work
with, and we are only using up around 10 right here. Even on the oldest systems,
10 bytes is not very much.

So instead of packing them all together, let's give each string it's own space
to live in. If we think about when we were maintaining the boxes, it would be as
if we stored the programs boxes in different areas of a warehouse instead of
just the 8 we had in front of us. In picture form:


```
   +===+====+                    +===+====+              +===+===+===+====+
   |'a'|'\0'|                    |'b'|'\0'|              |'a'|'r'|'g'|'\0'|
   +===+====+                    +===+====+              +===+===+===+====+
      \ /                           \ /                         \ /
+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+
| | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | |
+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+
```

I've squished things down a bit to fit in the picture, but hopefully it makes
sense. The big row across the bottom is a portion of memory. The boxes are not
specific units, they just to give a sense of the incredibly long list of empty
boxes that make up memory. Our strings are just tiny spots in the larger chunk
of memory.

Now that we've found ample space for our strings, we need to make sure we don't
lose them! To keep track of where everything is, we add an address system. Like
house numbers, every single byte of memory gets an address. To keep things
simple, we just number the bytes from 0 to the total number of bytes that the
memory can hold. Let's add some addresses to our data:


```
     37  38                       105  106                987 988 989  990
   +===+====+                    +===+====+              +===+===+===+====+
   |'a'|'\0'|                    |'b'|'\0'|              |'a'|'r'|'g'|'\0'|
   +===+====+                    +===+====+              +===+===+===+====+
      \ /                           \ /                         \ /
+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+
| | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | |
+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+
```

Great! Now, instead of storing the strings themselves, we can just store their
addresses. When I first introduced arrays, I added a little arrow at the
beginning. This arrow is exactly the same as our addresses. So now let's revisit
that picture with our new information.

```
       my_array = 37
          |
          V
          +========+========+
          |   'a'  |  '\0'  |
          +========+========+
                   ^
                   |
            my_array + 1*sizeof(char) = 38
```

So our previous picture was actually perfectly accurate, we just didn't have the
addresses. To make sure this system really works, let's try with `int`'s:
 
```
       my_array = 50
          |
          V
          +========+========+
          |   74   |   42   |
          +========+========+
                   ^
                   |
            my_array + 1*sizeof(int) = 54
```

For some hypothetical array `int my_array[] = { 74, 42 };`, our indexing system
works perfectly. Assuming `int`'s are 4 bytes long, if we start at 50 the next
`int` will be at address 54. Hopefully this makes you appreciate the simple
`array[1]` syntax! If you had to calculate the size inside every time and track
addresses, things would get complicated fast.

Back to our string problem, we need a way to describe the difference between a
value like `'a'`, and the address of that value, 37. This is why we introduce
the `*` symbol. Again, a picture should help.

```
   char *ptr               char x
   +========+            +========+
   |   37   | ---------->|   'a'  | 
   +========+            +========+
```

We call these addresses "pointers" because they "point" to the value in memory.
So `char *ptr = 37;` means that `ptr` is holding the address of some `char`. We
can now use `ptr` like an array. so:

```c
ptr[0] = 'a';   // The value at address 37
ptr[1] = '\0';  // The value at address 38 (because 37 + sizeof(char) = 38)
ptr[2] = ???;   // DANGER: This is past the end of our string! C let's us do
                // this, but the value is undefined. It can be anything!
```

Like we talked about before, there is no way to just know the length of a
string. But C is good about including the null characters on the end of them, so
we just need to check for them.

Given that example, we can see that `char *my_string` can be treated as a list
of characters, otherwise known as a string. We have the starting address of the
string, and we can walk down it `char` by `char` to find every element.

If a `char *` is a list of `char`'s, then a `char **` is a list of lists of
`char`'s. Or, said another way, a list of strings! Which is what we have been
looking for! This is a tough concept to explain in words, so here is another
picture:

```
   char **argv (pointer to list of strings)
     +====+
     | 60 |-------
     +====+      |
                 v  char *strs (pointers to lists of chars)
                 +===+===+===+
                 | 37|105|987|
                 +===+===+===+                             (list of chars)
     37  38       60   |   76     105  106                987 988 989  990
   +===+====+          |         +===+====+              +===+===+===+====+
   |'a'|'\0'|          |         |'b'|'\0'|              |'a'|'r'|'g'|'\0'|
   +===+====+          |         +===+====+              +===+===+===+====+
      \ /             \ /           \ /                         \ /
+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+
| | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | |
+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+
```

This is not the easiest picture to understand, however you should spend some
time with it. One major win we get from this pointer business is that `argv[1]`
is the second argument. Breaking that down should help give you a grasp on how
this works together.

We start with `char **argv`, a pointer to a pointer to a `char`. Then we
calculate `argv[1]`:

```
argv[1]
argv + 1*sizeof(char *)
60 + 8
68 (points to 105)
```

Pointers are all the same size, but that size depends on your computer. 4 and 8
byte pointers are common these days, but other sizes are possible. I'm going to
use 8 as a standard from now on, but know that it is not always the case.

So now we have a new pointer. Let's give it a name and say `char *arg2 =
argv[1];` Now let's try to determine what `arg2[0]` is:

```
arg2[0]
arg2 + 0 * sizeof(char *)
105 + 0
105 (points to 'b')
```

We've successfully made it down to `'b'`! And if we eliminate the intermediate
variable, it's actually a nice syntax: `argv[1][0]`. Read from right to left,
that says "take the first element of the second element of `argv`". Which,
filling in our types, says "take the first character of the second string of
`argv`". Which is pretty easy!

Note: Pointers are one of the biggest humps to get over when learning C. Don't
worry if you don't understand them just yet. We will use them non-stop going
forward, so you will get all the practice you could ever want.

[Stack vs. Heap](13-stack-vs-heap.html)
