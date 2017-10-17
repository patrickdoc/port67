---
title: Fibonacci
...

# Fibonacci

The fibonacci sequence is another classic programming task. It is useful because
it easy to understand and do the early parts by hand, but introduces a whole new
idea called "recursion". A recursive function is a function that calls itself in
its body. Let's state the fibonacci sequence equation and then see what that
means for our code.

```
fib(0) = 1
fib(1) = 1
fib(n) = fib(n-1) + fib(n-2)
```

For reference, here are the first 10 numbers in the fibonacci sequence.

```
1, 1, 2, 3, 5, 8, 13, 21, 34, 55
```

To get any number after the first two, you just add the previous two together.
Using comments to sketch out the program can help us determine exactly what
we need to do.

I'd suggest moving to a suitable place in your filesystem (like
`~/Projects/Python/`), creating a file called `fibo.py`, and following along.

```python
# fibo.py

# fib(0) = 1

# fib(1) = 1

# fib(n) = fib(n-1) + fib(n-2)
```

Hopefully, the first obvious thing to do is make a function called `fib` that
takes a single argument `n`.

```python
# fibo.py

def fib(n):
```

Now we can attack the tasks one at a time.

```python
# fibo.py

def fib(n):
    # fib(0) = 1
    if n == 0:
        return 1
```

Easy! The next one is just as easy using the `elif` keyword. `elif` is short
for `else if`, and it lets us chain together `if` statements. See the
[fizzbuzz](FizzBuzz.md) program for more information.

```python
# fibo.py

def fib(n):
    # fib(0) = 1
    if n == 0:
        return 1
    # fib(1) = 1
    elif n == 1:
        return 1
```

Perhaps surprisingly, our last case can be handled by just calling `fib` 
**inside** our `fib` function.

```python
# fibo.py

def fib(n):
    # fib(0) = 1
    if n == 0:
        return 1
    # fib(1) = 1
    elif n == 1:
        return 1
    # fib(n) = fib(n-1) + fib(n-2)
    else:
        return fib(n-1) + fib(n-2)

print fib(0)
print fib(1)
print fib(5)
print fib(9)
```

Testing our function and referring to our list above to check,

```bash
$ python fibo.py
1
1
8
55
```

Exactly what we expected! A word of warning though: recursive functions can be
a source of infinite loops. Consider these small programs,

```python
def loop(n)
    if n == 0:
        return 0
    else:
        return loop(n)

def loop2(n):
    if n == 0:
        return 0
    else:
        return loop2(n-2)
```

Without running them, try to figure out what they will do. In particular, try
to come up with return values for:

```python
loop(0)
loop(1)
loop(2)

loop2(0)
loop2(1)
loop2(2)
```
