---
title: "C Pointers"
subFiles: []
---

There's one piece of our basic C program template that we haven't addressed yet:
`char** argv`.

Suppose we wrote this bit of code,

```c
int main(int argc, char** argv) {
    char letter = 'Z';
    return 0;
}
```

It might look like this in memory,

```
     letter
        V
+=======+=======+=======+=======+
|  ---  |  'Z'  |  ---  |  ---  |
+=======+=======+=======+=======+
^       ^       ^       ^
0       1       2       3
```

Here we have 4 bytes of memory, where byte 1 contains 'Z'.

If we use the variable `letter` in our program, C will get the value, `'Z'`, and
substitute that into our program for us. So if we added this line to our
program,

```c
int main(int argc, char** argv) {
    char letter = 'Z';
    char otherLetter = letter;
    return 0;
}
```

C might update memory to look like this,

```
     letter         otherLetter
        V               V
+=======+=======+=======+=======+
|  ---  |  'Z'  |  ---  |  'Z'  |
+=======+=======+=======+=======+
^       ^       ^       ^
0       1       2       3
```

If we were to change the value of `letter`, `otherLetter` would still be `'Z'`.

```c
int main(int argc, char** argv) {
    char letter = 'Z';
    char otherLetter = letter;
    letter = 'A';
    return 0;
}
```

And memory would look like this,

```
     letter         otherLetter
        V               V
+=======+=======+=======+=======+
|  ---  |  'A'  |  ---  |  'Z'  |
+=======+=======+=======+=======+
^       ^       ^       ^
0       1       2       3
```

So far, so good. Things are working as expect them to.

We can do something a bit trickier though. The byte numbers in the picture above
aren't just for show. They are called "addresses". Each byte of memory has a
unique address, independent of the value stored in that byte.

C allows us to access the address of a variable with `&`. So while `letter` is
`'A'`, `&letter` is `1`.

This can be confusing because now each variable has two potential values. We
could be talking about the address, or we could be talking about the value
stored there.

Luckily, the type system helps us out here.

```c
int main(int argc, char** argv) {
    char letter = 'Z';
    char* letterAddress = &letter;
    return 0;
}
```

`letterAddress` has type `char*` instead of `char`. We read `char*` as "char
pointer" or "pointer to a char". It indicates that the variable holds the
_address_ of a `char` instead of an actual `char`.

And again, in memory,

```
     letter         letterAddress
        V               V
+=======+=======+=======+=======+
|  ---  |  'Z'  |  ---  |   1   |
+=======+=======+=======+=======+
^       ^       ^       ^
0       1       2       3
```

We also have to be able to go the other direction. Instead of going
from variable to address, we have to go from address back to the variable.
Just like we have `&` to go from `char` to `char*`, we have `*` to go from
`char*` to `char`.

Here is `*` in action,

```c
int main(int argc, char** argv) {
    char letter = 'Z';
    char* letterAddress = &letter;
    char otherLetter = *letterAddress;
    return 0;
}
```

```
   letter  otherLetter  letterAddress
        V       V       V
+=======+=======+=======+=======+
|  ---  |  'Z'  |  'Z'  |   1   |
+=======+=======+=======+=======+
^       ^       ^       ^
0       1       2       3
```

It is slightly unfortunate that `*` plays two different roles because it makes
it a bit harder to know which one you are working with. In types, it indicates
that the variable holds an address instead of a value. When used with a
variable, it gets the value stored at the address in the variable.

Each variable now has three possible values, so let's list them all out so you
can verify each one.

Given,

```c
char letter = 'Z';
char* letterAddress = &letter;
char otherLetter = *letterAddress;
```

We have the following 9 possible values,

```
letter = 'Z'
&letter = 1
*letter = ERROR
letterAddress = 1
&letterAddress = 3
*letterAddress = 'Z'
otherLetter = 'Z'
&otherLetter = 2
*otherLetter = ERROR
```

`*letter` and `*otherLetter` will cause errors if you try to use them because
`'Z'` is not a valid memory address. If you try to access an invalid address,
your program will crash to prevent bad things from happening to the rest of your
computer.

Our original question was about `char**` though, not just `char*`. A `char**` is
a pointer to a pointer to a `char`. Put another way, it is the address of a
`char*`. We could, for example, declare a new `char**` like this,

```c
int main(int argc, char** argv) {
    char letter = 'A';
    char* letterAddress = &letter;
    char** letterAddressAddress = &letterAddress;
}
```

Which might then look like this in memory,

```
   letter letterAddress letterAddressAddress
        V       V       V
+=======+=======+=======+=======+
|  ---  |  'B'  |   1   |   2   |
+=======+=======+=======+=======+
^       ^       ^       ^
0       1       2       3
```

This might seem a little silly right now, but pointers are surprisingly
powerful. They allow us to use memory very efficiently and process data
quickly.

We'll get plenty of practice and explore uses of pointers in the upcoming
topics. But until then, it would be good practice to read through this program
and make sure you understand all of the final values in memory.

```c
int main(int argc, char** argv) {
    char letter = 'A';
    char* letterAddress = &letter;
    char otherLetter = letter;
    letter = 'Z';
    otherLetter = *letterAddress;
    *letterAddress = 'B';
    return 0;
}
```

```
   letter  otherLetter  letterAddress
        V       V       V
+=======+=======+=======+=======+
|  ---  |  'B'  |  'Z'  |   1   |
+=======+=======+=======+=======+
^       ^       ^       ^
0       1       2       3
```
