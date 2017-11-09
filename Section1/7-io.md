---
title: Input and Output
section: Section1
prev: 6-executing-python.html
next: 8-types.html
description: Communicate with your program
...

# Input/Output

Last time we ended with this program,

```python
import math

def pythagorean_theorem(a, b):
    c = math.sqrt(a*a + b*b)
    return c

x = pythagorean_theorem(5, 13)
```

While this is certainly a good place to start, we have a couple problems. The
script as we have written it is not even as powerful as a calculator. All it
does is find one hypotenuse; it doesn't even tell us what the answer is! At the
moment, our best option is to jsut use the REPL and manually try things.  But
we have already written the code to solve for *any* hypotenuse, so we are only
lacking a good way to access it.

This is where the concept of Input/Output (I/O) comes in. Input is the process
of sending information into a program, and output is the program's ability to
put information out into the world.

## Output

Output is so important to computer science that most programming languages make
it easy. In Python, all we have to do is say `print`.

```python
import math

def pythagorean_theorem(a, b):
    c = math.sqrt(a*a + b*b)
    return c

x = pythagorean_theorem(5, 13)
print "Hello, world!"
print x
```

```note
Printing in Python is one of the most visible differences between
Python2 and Python3. In 2, we use `print stuff`. Whereas in Python3,
print became just a regular function: `print(stuff)`. All you need
to remember is to use parentheses for 3, and don't for 2.
```

Now we can ask python to read our file as input instead of typing into the REPL.
With a path as an argument to Python, it reads the file one line at a time,
exactly as it would if we were typing the file into the REPL line by line.

```bash
$ python pyt.py
Hello, world!
13.92838827718412
```

Welcome to the world of programming! "Hello, world!" is a traditional first
program in computer science. The phrase pops up all the time, and it just
represents an early success. Like this one! It is impossible to overstate how
impressive those nine lines of code are. With just the knowledge gained up to
this point, you could write some truly interesting programs. I'm obviously not
going to leave you hanging here, but give yourself a pat on the back.

# Input

But that's only half the battle. We now need to handle input. While this
program might not need a nice interface (that is, how the person interacts with
the program and vice versa), we are going to give it one just for fun. We are
going to expand the script to ask us for the two side values and then `print`
out the result. Luckily, the function that helps us do this is aptly named
`input`.

```python
import math

def pythagorean_theorem(a, b):
    c = math.sqrt(a*a + b*b)
    return c

a = input('Enter side a: ')
b = input('Enter side b: ')

print pythagorean_theorem(a, b)
```

```bash
$ python pyt.py
Enter side a: 10
Enter side b: 22
24.1660919472
```

And that is a nifty program. It might save you 10 minutes on homework if you
are still in high school. As a quick review to make sure this is all clear, we
are setting the variable `a` to the return value of the `input` function.
`input` takes a single argument, a string (the programming word for a sequence
of letters, spaces, punctuation, numbers, etc.), which it prints. Then it takes
everything you type before pressing `enter/return` and returns it. In our
example, `a` is set to 10, and `b` to 22. After that, it runs our function
on the two values and `print`'s the output. We could have set a variable `c`
to the hypotenuse length, and then `print c`, but this way also works.

With basic functions and I/O, you could theoretically write all sorts of cool
programs. But that's not our goal here (well, it is eventually, but not yet).
Next up, we are going to be talking about "things" in programming languages. In
particular, we are going to find out the difference between `a`, `'a'`,
`[a]`, and more.
