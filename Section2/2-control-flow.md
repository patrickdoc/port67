# Control Flow

Control flow is very similar in C to what we have seen in Python. The syntax is
a little bit different, but you'll start to get a feel for it.

# If

One of the clearest differences is the mandatory brackets and parenthesis.

```c
if (1 == 1) {
    printf("True!\n");
} else {
    printf("False\n");
}
```

The expression that `if` checks must be in parentheses, and instead of a `:`
with indentation, we use `{}` to block of the body of the `if`. And again,
after every statement in C, we place a `;`.

This brings us up against a fairly large user space difference between C and
Python. Python is whitespace sensitive. That is, it uses indentation and
newlines to help determine what you are asking of it. In C, whitespace can
almost always be removed. To see what I mean, here are multiple valid ways of
writing the above `if else` block.

```c
if (1 == 1) { printf("True!\n"); }
else { printf("False\n"); }

        if (1 == 1)
{ printf ("True!\n"); } else {
printf("False\n"); }

if(1==1){printf("True!\n");}else{printf("False\n");}
```

Hopefully you can see why we don't write code like this. While the computer is
happy to read this, most humans are not. I am not going to force you into any
particular style, but I am going to try and be consistent with my own. This
will help you read my code, which follows mostly standard rules. If you come
across another style that you like better, go for it. But never impose your
style on someone else's code.

As a quick example, there are two fairly popular ways of writing brackets for
functions and blocks.

```c
// The option I prefer
if (1 == 1) {
    printf("True!\n");
} else {
    printf("False\n");
}

// Another popular style
if (1 == 1)
{
    printf("True!\n");
}
else
{
    printf("False\n");
}
```

I personally think find the first version perfectly satisfactory, while other
people like having their `{}`'s line up. Once we get into longer programs, you
can see why this might come up. The number one rule of style though is to be
consistent. If it's your code, stick to one set of rules. If it is someone
else's, follow their lead.

One final note about `if` blocks in C: chaining them together is just writing
them next to each other. There is no special `elif` keyword. You just follow
an `else` with another `if`. Like so,

```c
if (x == 1) {
    printf("1\n");
} else if (x == 2) {
    printf("1\n");
} else if (x == 3) {
    printf("1\n");
}
```

# While

While loops are identical save the syntax. We just have to add in some `{}`'s.

```c
int x = 1
while (x < 10) {
    printf("Counting...\n");
    x++;
}
```

I've thrown in a new operator here too. While C does support the `x += 1`
method of incrementing `x`, adding 1 to a variable is so common that it has
special syntax. `x++` increases the value of `x` by 1.

# For

For loops are fairly different in C. They are much closer to a while loop than
to Python's for loop. When we start using them more heavily, we will see why
Python went the way it did. For now though, let's look at an example.

```c
for (int x = 1; x < 10; x++) {
    printf("Counting...\n");
}
```

This code is functionally identical to the while loop above. We just packed all
of the information into the first expression. There isn't a great way to read
a for loop into English, but it works like this: starting with `x = 1`, check
if `x < 10`. If it is, then run the body of the loop followed by `x++`. If `x`
is not less than 10, continue on past the loop. For now, it's best to see this
as a better way of writing the `while` loops we have been writing that
increment some variable before looping again.
