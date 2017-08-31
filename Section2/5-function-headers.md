# Function Headers

If we were limited to only using a `main` function, C would become unreadable
extremely quickly. Just like in Python, we can create functions in C to help us
organize and reuse our code. Function definitions look very similar to the
`main` function,

```c
float circumference(float radius) {
    return 2 * radius * 3.14159;
}
```

Remembering the patterns from `main`, we can see that the function called
`circumference` takes a `float` and binds it to the variable `radius` and
returns a `float`.

To use our function inside `main`, we need to "call" it. Calling a function
looks similar to the definition, but we drop the type annotations (i.e.
`float`).

```c
float circumference(float radius) {
    return 2 * radius * 3.14159;
}

int main(int argc, char **argv) {
    float x;
    x = circumference(2.43);
    return 0;
}
```

You might be tempted to `return x` here, but remember that the return value of
main indicates the success or failure of the program. Because there isn't
really anything that could go wrong, this program always returns 0.

An interesting feature of functions is that they can be declared with no
value, just like variables. Typically, a declaration just looks like a normal
definition, without the `{}`'s.

```c
float circumference(float radius);
```

Just like we declared the type of a variable without giving it a value (e.g.
`float x`) we declared the type of a function without giving it a value.
The type of `circumference` is: given a `float`, return a `float`.

One of the first powers we gain from this is the ability to move code around.
Often, I like to have my `main` function at the top. But if it relies on other
functions in the file, I have to put it below them. With these function
"headers" or declarations, I can put the bodies below my `main` function.

```c
float circumference(float radius);

int main(int argc, char **argv) {
    float x;
    x = circumference(2.43);
    return 0;
}

float circumference(float radius) {
    return 2 * radius * 3.14159;
}
```

Notice there is a subtle difference here between variable declarations and
function declarations. With variables, we have to be very careful to make sure
that the variable has a value before we try to use it. With functions, we can
use the function above where we define it in the code. This is because the
compiler reads all your code, and can match up function declarations with
bodies. But the value of your variable isn't known until the code is run.

I don't expect you to fully understand how this works right now because I
haven't even begun talking about compilers. But it's always good to leave some
questions open for you to think about and work through on your own. As an
exercise, pretend you are a compiler. Looking at the code, you are welcome to
write down any information you want about this program. Using that information,
see what you can simplify about it. (A great compiler would just simplify this
entire program to `return 0`. The key observation being that `x` is never used.
Ignoring that option, there are still plenty of places you could improve).

While the above program can be optimized in many places, the following code
cannot. Think about the differences between `x` and `y` and what that means for
the compiler.

```c
float circumference(float radius);

int main(int argc, char **argv) {
    float y;
    if (argc <= 1) {
        y = circumference(2.43);
    } else {
        y = circumference(argc);
    }
    return 0;
}

float circumference(float radius) {
    return 2 * radius * 3.14159;
}
```
