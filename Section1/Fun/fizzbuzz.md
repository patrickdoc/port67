---
title: FizzBuzz
...

# FizzBuzz

The `FizzBuzz` problem is a classic exercise in CS. It is based on a children's
word game that helps them learn math. But for us, it will help us work on our
control flow. It goes like this:

```
Write a program that prints the numbers 1 to 100. However, if the number is
divisible by 3, print *fizz*. If the number is divisible by 5, print *buzz*.
And if it is divisible by both, print *fizzbuzz*.
```

Any time you are going to write any code, you start by opening up your terminal,
navigating to a reasonable place (e.g. `~/Projects/Python/fun`), and opening a
new file (like, `fizzbuzz.py`).

One way we could start is by outlining the problem with comments. In this case,
we have four reasonably clear tasks.

```python
# FizzBuzz.py

# Print the numbers 1 - 100

# Print numbers divisible by 3 as fizz

# Print numbers divisible by 5 as buzz

# Print numbers divisible by 3 and 5 as fizzbuzz
```

We now have a good place to start: printing the numbers 1-100. I'm only going to
write out pieces, or snippets, of the file here at any given time to save space.
But I will do my best to make it easy to know where things should go.

Thinking back to the [control flow](../ControlFlow.md) lesson, we actually did
something like this already.

```python
x = 1
while (x <= 1000):
    print x
    x += 1
```

With some tweaking, this will work nicely.

```python
# Print the numbers 1 - 100
x = 1
while (x <= 100):
    print x
    x += 1
```

Ending the `while` loop earlier tackles the first task. If we now save and quit
(`:wq`) we can test the program.

```bash
$ python fizzbuzz.py
1
2
3
...
100
```

Great! Now onto the second task: if a number is divisible by 3, print fizz. We
can almost translate this directly from English into Python.

```python
# Print the numbers 1 - 100
x = 1
while (x <= 100):
    # Print numbers divisible by 3 as fizz
    if (x % 3 == 0):
        print 'fizz'
    else:
        print x
    x += 1
```

The `%` operator does division, and returns the remainder. For example,
`10 % 3` is 1. Divisible by 3 means the same thing as having 0 remainder after
division, so we check that.

Testing again:

```bash
$ python fizzbuzz.py
1
2
fizz
4
...
fizz
100
```

Still looking good! The third task works the same way, with a slight twist so
both checks work.

```python
# Print the numbers 1 - 100
x = 1
while (x <= 100):

    # Print numbers divisible by 3 as fizz
    if (x % 3 == 0):
        print 'fizz'
    # Print numbers divisible by 5 as buzz
    elif (x % 5 == 0):
        print 'buzz'
    else:
        print x

    x += 1
```

`elif` is short for `else if`, and is a way to chain together `if` statements.
This `if` block can be read "If x is divisible by 3, print fizz. If it is not
divisible by 3, but is divisible by 5, print buzz. If none of the above, print
x". We can have multiple `elif`'s to check multiple different cases. In fact,
we will need a second to complete this program. I've also added some whitespace
to make the program a little easier on the eyes.

```bash
$ python fizzbuzz.py
1
2
fizz
4
buzz
...
13
14
fizz
16
...
fizz
buzz
```

Our program is almost there, but we haven't handled the case where a number is
divisible by both 3 and 5. 15 is the first number divisible by both, and as we
can see in the output, we are only printing fizz. To see why, let's think about
the program. If a number is divisible by 3, our `if` block will print fizz
without ever checking if it is also divisible by 5.

To finish off our program, we just need to check the fizzbuzz case first.

```python
# Print the numbers 1 - 100
x = 1
while (x <= 100):

    # Print numbers divisible by 3 and 5 as fizzbuzz
    if (x % 3 == 0) and (x % 5 == 0):
        print 'fizzbuzz'
    # Print numbers divisible by 3 as fizz
    elif (x % 3 == 0):
        print 'fizz'
    # Print numbers divisible by 5 as buzz
    elif (x % 5 == 0):
        print 'buzz'
    else:
        print x

    x += 1
```

`and` is a function that does exactly what its name says. It is true if the
first `and` the second arguments are true. Otherwise it is false.

And finally,

```bash
$ python fizzbuzz.py
1
2
fizz
4
buzz
...
13
14
fizzbuzz
16
...
fizz
buzz
```

## Extras

There are as many ways to write a program as there are to skin a cat. The one
presented here fits best with what we know at this point. To explore another
and practice reading documentation, try replacing the `x` and `while` loop with
a `for` loop using the `range` function. (LINK TO RANGE AND 2/3 DIFFERENCES)

The `elif` keyword is a blessing for us. It actually can be expanded using
`if` and `else`. To see why the short version is so much better, try expanding
our final `if` block using this rule.

```python
if x:
    print x
elif y:
    print y
else:
    print z

# The above is the same as:

if x:
    print x
else:
    if y:
        print y
    else:
        print z
```

Even just the two `elif` block that we use above gets out of hand. But I would
encourage you to write it out and think up some test cases (maybe 1 and 3 to
start). Working through test cases is a great way to double check that you know
what your code is doing.
