# FizzBuzz (Fill-in)

Problem statement:

```
Write a program that prints the numbers 1 to 100. However, if the number is
divisible by 3, print *fizz*. If the number is divisible by 5, print *buzz*.
And if it is divisible by both, print *fizzbuzz*.
```

You may find the `%` operator useful. `10 % 4` finds the remainder of 10
divided by 4, which is 2. For any two numbers `a` and `b`, `a` is "divisible"
by `b` exactly when the remainder is 0.

You may also want the `and` operator, which is true when both its first and
second arguments are true, and false otherwise. So `True and True` is `True`,
but any other combination `False`.

`fizzbuzz.py`

```python
# Print the numbers 1 - 100
x = 1
while (____):

    # Print numbers divisible by 3 and 5 as fizzbuzz
    if ____:
        print 'fizzbuzz'
    # Print numbers divisible by 3 as fizz
    elif ____:
        print 'fizz'
    # Print numbers divisible by 5 as buzz
    elif ____:
        print 'buzz'
    else:
        print x

    # Update counter
    ____
```

## Extras

There are as many ways to write a program as there are to skin a cat. The one
presented here fits best with what we know at this point. To explore another
and practice reading documentation, try replacing the `x` and `while` loop with
a `for` loop using the `range` function. (LINK TO RANGE AND 2/3 DIFFERENCES)
