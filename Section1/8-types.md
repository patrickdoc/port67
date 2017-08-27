# Types

One of the features that differentiates programming languages is how they
handle "types". Type systems are a set of rules that control how you are
allowed to use different values. When you tell the computer to run your program,
it doesn't check to make sure that what you are doing is sensible. It just runs
your set of instructions until it reaches the end or causes an error. Type
systems help diminish the chance of the runtime errors and provide extra
information in error messages.

```python
>>> w = 0
>>> x = 1
>>> y = 2
>>> z = 'hello'
>>> print z
hello
>>> print x + y
3
>>> print y + z
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: unsupported operand type(s) for +: 'int' and 'str'
```

This example demonstrates a basic programming mistake that a type system can
catch. Accidentally using the wrong variable is often a source of difficult to
find bugs. Luckily, types help us in many cases, like this one! Let's look at
what the error is telling us,

```
TypeError: unsupported operand type(s) for +: 'int' and 'str'
```

There are a number of new concepts here. First, Python is telling us that there
was a ``TypeError``. That means that one of the rule of the type system was
violated. Next it will tell us which rule, ``unsupported operand type(s) for
+``. An "operand" is an argument to an "operator", in this case ``+`` is our
"operator" and ``y``/``z`` are our "operands". The error is that ``+`` expects
its "operands" to have certain types, but ``y`` and ``z`` don't match those.
Finally, it tells us that the types that failed are ``'int'`` and ``'str'``.

So what are ``'int'`` and ``'str'``? They are specific types. ``'int'`` stands
for Integer. The integers are the Natural numbers (or counting numbers) and
their negatives. That is, ..., -2, -1, 0, 1, 2, ... From negative infinity to
infinity. (They are actually bounded above and below due to hardware
restrictions, but we will get to that when we talk about representation of
types in C.)

``'str'`` is the String type. Strings are pieces of text. For example,
``'hello'`` is a string. We also have ``''`` the empty string, or the entire
text of Moby Dick. You can think of Strings as a sequence of 0 or more
Characters, where a Character is a letter, number, space, or punctuation.
Python does not have a specific type for individual characters, but many other
languages do.

One final type we can talk about here is Float, or floating-point numbers.
These are all the rational numbers, which can be represented as fractions.
However, fractions are not a great representation for the computer to hold, so
we use floating-points instead. These are numbers like 0.5 (one half), or
82934.123845132452, or 1.0.

With this knowledge in hand, the error is slightly clearer. We are trying to
add an Integer (..., -1, 0, 1, ...) to a String. But that doesn't make much
sense. Adding two values of the same type is usually fairly easy to define.
With numbers, it is just simple addition. Take a guess what happens when you try
to add two strings, and test it out in the REPL. But how should the computer
add a string to an int? There really isn't a sensible answer, and so we get an
error.

Sometimes different types can actually work well together though. What happens
when you try to add an int and a float? How do these types relate to each
other? You can always turn an int like ``2`` into a float by just adding a
``.0`` to the end. But can you turn a float like ``0.5`` into an int? You could
by just dropping everything after the decimal, but that is not a great solution.
So let's convert our int into a float and proceed as normal. Thus the type of
``'int' + 'float'`` is ``'float'``.

If you were to try out all the different combinations of types and addition,
you would find some errors and some values. The type system is the tool we use
to determine whether or not some calculation makes sense. Often, the rules of
the type system catch simple errors that you made. It is unlikely that we meant
to add an int and a string above. More reasonably, we meant to add ``w + x``.

While the type system caught our mistake here, we can also take this as an
opportunity to learn a good habit. Naming variables meaningfully is a good
practice to get into. We could have avoided this mistake by using better names.

```python
>>> cost_of_hug = 0
>>> cost_of_banana = 1
>>> cost_of_orange = 2
>>> greeting = 'hello'
>>> print greeting
hello
>>> print cost_of_banana + cost_of_orange
3
>>> print cost_of_orange + greeting
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: unsupported operand type(s) for +: 'int' and 'str'
```

Notice that our error is easy to see without even running the code.
