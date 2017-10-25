---
title: Strings
...

# Strings

C does not have a special `string` type. Instead, it just uses a list of
`char`'s. However, C provides a number of functions and features that make
working with strings easier. String are central to many aspects of programming,
so it's important to get to know them (and their pitfalls).

`printf` uses one of these features. Using the tools from last time, let's
create a string:

```c
char my_string[] = "Hello";

printf("my_string contains the string: %s\n", my_string);
```

This is a little different than what we have seen. First, we didn't give a
number for the initial size of the array. Because we gave a value for the array
immediately, the compiler can figure out how big the array should be. Second, we
initialized the array with a string literal (i.e. some characters between
quotes), instead of the `{}` notation we learned last time. This is part of the
machinery to make strings a little easier. When we get a bit farther, we can dig
into why this trick works.

If we supply the `include` and `main` boilerplate code, our output looks like:

```bash
my_string contains the string: Hello
```

Imagine for a moment what `my_string` looks like in memory:

                       my_string
    +========+========+========+========+========+
    |  'H'   |   'e'  |   'l'  |   'l'  |   'o'  |
    +========+========+========+========+========+

Here, I've used our shorten form from before, so each box represents a `char`,
8 bits, or 1 byte (all the same). I've also filled in the values. Now imagine we
are writing the code for `printf`. We are given the array of `char`'s called
`my_string`, and we are asked to print out all the values in it. How do we do
it?

If we were asked to print a single character, it would be easy because we know
that a `char` is always 1 byte. But arrays can be any size! If you've thought a
bit about compilers, you might say that the compiler should be able to know how
long the array is. There's a sticking point though: we are not required to
actually use all the space we ask for.

Suppose we have a variable called `my_name` that will hold a user's name. This
will be a string, but how big? My first name requires seven letters. But someone
named Alice would only need 5. Instead of trying to fit these names perfectly,
we can just declare:

```c
char my_name[50];
```

That way we set aside enough space to hold any name. But we won't always use the
entire space. And when we call `printf` on `my_name`, we want it to print out
just my name without any of the extra garbage that is in the array after it.

(Interestingly, it turns out that 50 probably isn't enough to hold even just
first names. Many large companies have written about how hard it is to deal with
names from around the world. Properly handling every name is not an easy task!)

To handle this, we set up a conventional rule. We promise to always signal the
end of our data with a null byte, a byte composed of all zeros. So our picture
from before is actually incorrect. Our `"Hello"` actually looks like this in
memory.

                       my_string
    +========+========+========+========+========+========+
    |  'H'   |   'e'  |   'l'  |   'l'  |   'o'  |  '\0'  |
    +========+========+========+========+========+========+

Just like `\n`, `\0` is a special character. It means the byte composed entirely
of zeros. The slash is important. If we didn't have it, we would never be able
to print the character 0. To maybe make this clearer:

```c
char zero_string[] = "After 0, things print, but after \0 not";
printf("%s\n", zero_string);
```

Running this, we see:

```bash
After 0, things print, but after 
```

The end of our string is cut off. C saw our null character and treated it as the
end of the string. Our string is just an array, so let's try to read the value
at the end. Remembering how array access works from last time, we want the sixth
character, so we use `my_string[6-1]` to access it:

```c
printf("The final byte is %d!\n", my_string[5]);
```

Now I cheated a little bit here. I used `%d` even though I wanted to print out a
`char`. The issue is that `%c` doesn't print anything when it is given a null
byte. When we read that block as an `int` using `%d`, we see that the byte is in
fact all 0's. Try with `%c` to see what happens.

Strings are a tricky part of C. They are the source of many *huge* security
issues. However, knowing how and why everything works is the best way I know of
avoiding these traps. Whenever you have an issue with strings, you should ask
yourself if you are holding up your end of the bargain. Do all of your strings
end with `\0`? Does your string have a null byte earlier than it should?
Sometimes the compiler knows where to put them to help you, but you should
always think about your ending null bytes when working in C.

[Pointers](12-pointers.html)
