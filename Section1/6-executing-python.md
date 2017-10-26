---
title: Executing Python
...

# Executing a Python File

So far we have just used a Python interpreter, but Python is more traditionally
a scripting language. That is, it excels at short programs that help with
certain tasks or problems. We are going to write a short program that solves
the Pythagorean Theorem: `a^2 + b^2 = c^2`. Solving for the hypotenuse of a
right triangle is a surprisingly good example for us. It is straightforward,
often too difficult to do manually, and extremely useful in computer science.
The Pythagorean Theorem comes up in graphics, physics, game logic, AI, and
more.

A brief overview of the equation will help us determine how to structure our
program:

      |\
      | \ c
    a |  \
      |___\
        b

Working from my stunning graphic, we have the two sides, of length `a` and `b`,
as well as a hypotenuse of size `c`. We really want to find `c` and not `c^2`,
so let's rearrange the equation a little bit: `sqrt(a*a + b*b) = c`. If we
flip the sides (`c = sqrt(a*a + b*b)`), we have something awfully similar to
a variable assignment.

Thinking back to our circumference example, our variables fall into two groups:
those we are given (often called parameters or arguments), and those we want
to find. In this case, `a` and `b` are going to be our arguments, and `c` our
return value. Now that we know where everything goes, our function mostly just
falls into place.

Using our knowledge of Vim, we can write and save a function in a file. After
that, we will be able to use it whenever we want without writing it again. If
you haven't already, now would be a good time to start setting up a directory
to use for work. I like `~/Projects`, but it is completely up to you. In the
future I won't explicitly tell you what to do with your files, but:

```bash
$ cd
$ mkdir Projects
$ cd Projects
$ mkdir Python
$ cd Python
$ touch pyt.py
$ vim pyt.py
```

We now have a file, `pyt.py`, to work in and a way to edit it, Vim. So let's
write our function:

```python
def pythagorean_theorem(a, b):
    c = sqrt(a*a + b*b)
    return c
```

This looks good, except for one slight problem. The function `sqrt()` is not
actually available to us. Most programming languages have it, but not always
immediately accessible. Languages often come with "standard libraries", which
are essentially big collections of functions that you will probably need at
some point. We need to ask Python to go get some of that code for us because
`sqrt()` is there. To do that we only have to make a small addition to our
program.

## Importing

```python
import math

def pythagorean_theorem(a, b):
    c = math.sqrt(a*a + b*b)
    return c
```

The `import math` statement at the top of the file tells Python that we would
like to use functions from the math library. When we then use those functions,
we add `math.` to the beginning to specify where it comes from. This is, in
part, to make sure that there aren't name collisions. If we had defined a
function called `sqrt()`, it would no longer be clear what we were referring
to. Do we mean the one we wrote? Or do we mean the one from `math`? Using
`math.` makes it perfectly clear what we intend.

We can now save and quit. (You could even do a `cat pyt.py` to make sure
everything is still there.) Our program doesn't tell Python to do anything,
it just defines a new function that we *could* use if we wanted to. So
asking Python to run it wouldn't be productive at this point. But it would be
nice if we could play around with our new function a little bit. So let's open
up the REPL with `python`.

```python
>>> import pyt
>>> pyt.pythagorean_theorem(3, 4)
5.0
>>> pyt.pythagorean_theorem(5, 12)
13.0
>>> pyt.pythagorean_theorem(5, 13)
13.92838827718412
```

`import pyt` works exactly like `import math`. This time though, Python
found pyt from the file `pyt.py` instead of from the libraries it comes
with. And again, we add `pyt.` to the beginning of our function calls.
After a few tests, it seems our function works correctly! We can double check
our answers with a calculator or by hand if you are a very brave soul.

Let's make one more change to our program. So far it just has some definitions,
but we want to make it do actual work.

```python
import math

def pythagorean_theorem(a, b):
    c = math.sqrt(a*a + b*b)
    return c

x = pythagorean_theorem(5, 13)
```

And again into the REPL,

```python
>>> import pyt
>>> pyt.pythagorean_theorem(5, 13)
13.92838827718412
>>> pyt.x
13.92838827718412
```

We've successfully made Python find the hypotenuse of a triangle all by itself.
Unfortunately, we need one more piece before our script is really useful. Right
now it calculates the answer, but doesn't do anything with it. Next time, we
are going to learn how to talk to Python and make it talk back.

# Next
[Input Output](7-io.html)
