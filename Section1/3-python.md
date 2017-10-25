---
title: Python
...

# Python

Now that we have enough knowledge to be dangerous, we can try out our first
programming language: Python. This is an ideal starting language for a few
reasons. It comes pre-installed on most systems these days. It excels at small
utility programs. And it is has many of the most common features among
programming languages, so it is a good representative of a large number of them.

```bash
$ python
Python 2.7.13 (<stuff>)
[<more stuff>] blah
<some instructions>
>>>
```

The very first information printed ("Python X.Y.Z") is the ``version``
information. The numbers indicate decreasing levels of change, so ``X.Y.7`` and
``X.Y.8`` would be very similar, while ``2.Y.Z`` and ``3.Y.Z`` would have major
differeces.  One downside of Python is that (at the time of writing) it is in a
transition period from Python 2 (i.e. versions like ``2.X.Y``) to Python 3.
Most systems come with Python 2 installed, but most people are moving to
3. We won't run into many of the differences right now, so it doesn't matter
which you have. However, when changes do pop up, I will do my best to make a
note of them. (If you don't have it... cry?)

So let's start playing with Python. You'll note that the prompt has changed
from `$` to `>>>`. This is a handy way to know that we have left bash and
moved into Python. This means that what we type will be interpreted as Python
code and not bash commands. Thus, if you were to try something like ``ls`` here,
you would get an error. So what should you try? Well, luckily, most programming
languages read math the way that you are used to:

```python
>>> 1 + 1
2
>>> 2+1
3
>>> 10 *12
120
>>> (351 / 9) + (-2 + 10 / 2)
42
```

Great, we have found a secret calculator on your computer! Computers were, to
a large extent, created to do math. As a result, they are very good at it.
However, they can do much more than simple arithmetic. We are going to look at
two features in particular that will become invaluable tools: variables and
functions.

## Variables

One of the goals of programming languages is to allow the programmer to think
about problems at the highest level. Let's say we want to calculate the
circumference of a circle. In a math class, you would talk about pi and
diameters, but a computer doesn't know what those things are. This is where
Python comes in. We tell Python what pi and diameters are, and then it
translates it into something that the next program down on the stack
understands. And this happens multiple times until something turns words into
numbers and numbers into electricity.

To define a new variable, you simply give it a name and a value like this:

```python
>>> pi = 3.14159
```

And now Python knows what pi is. If we ask it, we can see that it remembers.

```python
>>> pi
3.14159
```

Now let's define a diameter and find the circumference.

```python
>>> d=6
>>> pi * d
18.849539999999998
>>> c = pi * d
>>> c
18.849539999999998
```

We can see that we can use variables to set the value of other variables. But
what happens if you try to set a variable to a new value?

```python
>>> d = 12
>>> d
12
```

We have now changed the value of `d` from 6 to 12. So what happens to `c`?
Well let's check:

```python
>>> c
18.849539999999998
```

So even though `c` was defined with `d`, changing `d` did not affect
`c`. One way to think about this is to imagine Python simplifying when you
define `c`, like this:

```python
>>> c = pi * d
>>> c = 3.14159 * d
>>> c = 3.14159 * 6
>>> c = 18.849539999999998
```

So our definition of `c` does not actually include `d`.

## Functions

The equation for the circumference of a circle is not terribly complicated, but
even something as simple as this can get tedious after a bit. Each time you have
to define both `pi` and `d` and then muliply them together. That's at least
3 separate lines of code, excluding anything we do with `c`. To simplify this
process, we use our second tool, the function.

Let's think about the variables in our equation for circumference, `c = pi *
d`. `pi` is a constant. No matter where it is, it will always be `3.14159`.
`c` is the value we want to find, so all our work should lead to finding it.
Finally, `d` is the important variable, which could be different every time
we want to find a circumference.

Here is how we define and use a function to calculate the circumference:

```python
>>> def circumference(d):
...     pi = 3.14159
...     c = pi * d
...     return c
...
>>> circumference(1)
3.14159
>>> circumference(42)
131.94678
```

We've already seen variable definition, so we really only have two lines to
discuss. The first is `def circumference(d):` and the second is `return c`.
When we want to define a new function, we use this syntax:

```python
def function_name(variables):
```

The line always starts with `def` to inform Python that we want to make a new
function. After that, you pick a name for your function. It is standard to use
all lower-case letters and an underscore to separate words. Next come the
variables that the function needs to run. In the previous example, the final
value only needed a `d`, but sometimes you need more. If you want to ask for
more variables, you can use commas to separate them, like: `(a, b, x)`.
And finally, we end with a colon.

After the declaration of our new function follows the function body. This is
composed of all the indented lines directly below. Whenever you call the
function (using `function_name(variables)` as demonstrated above) the lines of
the body are executed in order. The final line of the body is something we
haven't seen before. `return` is a special keyword used within functions. In
short, it says what the value of the function is going to be.  If we were to
substitute `return 1` for `return c`, then any time we call the function it
would just answer with `1`.

```note
There is much debate over sizes of indent, but I've found
four spaces to work well across a wide variety of settings.
It is also the recommendation of the Python style guide.
```

Our function is very easy for a person to read: all of the variables are clearly
defined; they have informative names; and we can tell that calculating ``c`` is
the purpose of this function. We can make this function more concise though:

```python
>>> def circumference(d):
...     return 3.14159 * d
...
>>> circumference(1)
3.14159
>>> circumference(42)
131.94678
```

This brings up one of the core problems of computer science: there are hundreds
of ways to write any program, so how do you choose? As with most choices, there
are tradeoffs that you have to balance. On the one hand, for someone who doesn't
know the value of `pi` off the top of their head, having the value 3.14159
named makes it easy to understand what is happening. On the other, having to
write `diameter` mulitple times can be a bit of a pain, so substituting `d`
is simpler without losing too much information. Many people have come back to
code that they wrote months (or even days) ago, and had no idea what was
happening. A way to help choose might be to ask yourself, "If I come back to
this next month, will I know what `d` stands for?" I would even recommend
occasionally looking back at things you've written to practice reading code and
maybe even find some of your bad habits.

## Moving On

We are going to quit out of Python for a moment to motivate our next topic.
You can stop Python by either typing `Ctrl-D` (Control and d at the same
time) or by typing `quit()`. If we start up python again, we will find
something upsetting. All of our hard work is gone! `pi` is no longer defined
and we can't call our `circumference` function.

This is because we were just using a Python Read-Evaluate-Print-Loop (REPL).
This interactive mode just runs the code you give it and then waits for more.
If we want to save our work, we are going to need to put it somewhere permanent
and then tell Python where it is. Luckily, we have permanent storage called the
filesystem! Next time, we are going to create a file and fill it with Python
code.

[Files](4-files.html)
