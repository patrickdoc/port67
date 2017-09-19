# Control Flow

Our programs so far have been somewhat lackluster. One of the most glaring
issues is that they do exactly the same thing every time. The program that
asked us for the sides of a triangle before printing the hypotenuse could give
different answers, but was still somewhat boring.

In all the code we have written so far, Python has simply executed every line
from top to bottom, in order. You could imagine a small pointer moving down line
by line, indicating which line Python is executing. (This pointer does in fact
exist inside the computer and is called the ``program counter``.) The way this
pointer moves is called ``control flow``, and we are now going to learn how to
make it do more than fall from top to bottom.

## If Statements

Our first tool to affect control flow is the ``if`` statement.

```python
if x:
    print "x is True"
```

In many cases, names in computer science are not terribly helpful, but in this
case reading this code aloud should tell you what it does: "if x (is True),
(then) print "x is True"".

But what is ``x``? Remembering types from last time, ``x`` is anything that
can be converted into a Boolen (or truth value). There is a function ``==``
which checks if two things are equal to each other. (It is easy to confuse
``=`` and ``==``. The first sets the value of a variable, and the second
compares two values). ``if x:`` works by evaluating ``x == True``. This might
look like ``(1 > 0) == True`` which then transforms into ``True == True``, and
finally to ``True``. If we were to flip the 1 and 0, we would have ``(0 > 1)
== True``, which evaluates to ``False == True``, and finally ``False``.

```note
I would suggest playing with ``==`` in the REPL, as
it often has unexpected values. For instance, try comparing all the different
pairs of ``True``/``1``/``1.0``/``0``. ``1 == 1.0`` gives you True, which is
good because they really are the same number. But try checking ``1 == True`` and
``1.0 == True``.
```

This brings up a point of style. While ``if 1:`` is valid Python, it is not
the most readable Python. For the sake of those who have to read your code and
understand it (including you!), try to be explicit about what you are checking.
In this case, ``if 1 != 0:`` or ``if 1 < 23:`` might be better options. (``!=``
is read "not equal", and is the opposite of ``==``.)

We can also add a way to react when ``x`` is not ``True``.

```python
if x:
    print "x is True"
else:
    print "x is False"
```

## While

Sometimes you find yourself in a situation where you want to perform a certain
piece of code many times until some condition is true. For example, let's print
the numbers 1-1000:

```python
x = 1
while (x <= 1000):
    print x
    x += 1
```

This is called a "while loop". It starts by checking the condition, "is x less
than or equal to 1000". Since this is true for ``x == 1``, we run the code
inside. Then we loop back and check the condition again.

Because we are in this loop, we have to be careful to make sure that the
condition will eventually be false. Otherwise we will get caught in an infinite
loop. Try removing the ``x += 1`` line. Before running the code, guess what will
happen. This is a good habit to get into as it helps you find the differences
between how you think it will work and the bugs it has.

Once you run the program, it will just print ``1``'s infinitely. When you
realize your code has a bug or you forgot something, you can stop a running
program by typing Ctrl-C (Control and C together). This sends a message to
your program that says "stop what you are doing right now". Try not to let
infinite programs run for too long, as they just make your computer do useless
work and burn through your battery/electricity bill.

## For

Our final control flow statement is ``for``. It is similar to ``while``, except
you do not change the values of the variable. ``for`` is used to loop over the
values in a collection. In Python, we can create a list of object using this
format,

```python
[1,2,3]
```

This represents the numbers 1, 2, and 3 in that order. This is a new type,
called ``'list'``. Python's lists can contain any number of values (including 0,
the empty list []), be they variables, strings, ints, floats, etc.

The ``for`` statement then looks like this,

```python
for x in [1,3,4]:
    print x
```

One at a time, the variable will be bound to ``x``, and processed in the loop.
This is convenient when you know exactly what you want to loop over.

# An Example

To practice reading code and see some of these statements in action, I've
written up the comp sci 101 classic "Guess a Number" game. Read through this
code and try to understand how it works before running it. Additionally, look
at the Python
[documentation](https://docs.python.org/2/library/random.html#random.randint)
for the ``randint`` function I imported. Learning to read (and write!)
documentation will be one of the best investments you can make.

```python
import random

def main():
    # Secret is the number that the player is trying to guess
    secret = random.randint(1, 10)

    print "I'm thinking of a number between 1 and 10"

    num_guesses = 3
    guessed_vals = []
    while (num_guesses > 0):

        # We don't want the player wasting guesses they have already
        # tried, so we tell them what their earlier guesses were.
        for g in guessed_vals:
            print "You have guessed: ", g

        print "You have ", num_guesses, " guesses left"

        x = input("Guess: ")

        if x < secret:
            print "Too low!"
        # "Else if" lets us chain together multiple if statements
        elif x > secret:
            print "Too high!"
        else:
            print "You got it!"
            # Immediately exit the while loop because the game is over
            break

        num_guesses -= 1
        guessed_vals.append(x)

main()
```

As a matter of good practice, I have added comments to my code. Some are to
help understand how it works, like the explanation of ``break`` and ``elif``.
Others explain why I chose to do something, like print the previous guesses.

This game has lots of area for improvement. Some things to think about and
improve: what happens when the player runs out of guesses? what if the player
wants to play again? Could you make the previous guess information more useful
by also telling the player if the guess was too high or too low? Working through
these kinds of problems is the difference between good code and bad code.
Interestingly, it is also the difference between a good game and a bad game.
These exercises will also force you to work through some of the most common bugs
and tough spots in programs.

We aren't looking for pretty code just yet! Learning how to make code readable
requires seeing a large amount of unreadable code. That way you can see for
yourself what does and doesn't work. For now the most important idea is
understanding how the program runs. Once you get a good handle on that, you can
start to see patterns and apply some of the tools that people have made to help.
