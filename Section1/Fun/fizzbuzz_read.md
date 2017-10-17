---
title: FizzBuzz (Reading)
...

# FizzBuzz (Reading)

Problem statement:

```
Write a program that prints the numbers 1 to 100. However, if the number is
divisible by 3, print *fizz*. If the number is divisible by 5, print *buzz*.
And if it is divisible by both, print *fizzbuzz*.
```

`fizzbuzz.py`

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

## Extras

There are as many ways to write a program as there are to skin a cat. The one
presented here fits best with what we know at this point. To explore another
and practice reading documentation, try replacing the `x` and `while` loop with
a `for` loop using the `range` function. (LINK TO RANGE AND 2/3 DIFFERENCES)
