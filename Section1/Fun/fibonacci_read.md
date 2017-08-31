# Fibonacci (Reading)

Problem statement:

```
Write a function fib(n) that returns a value following the specification:

fib(0) = 1
fib(1) = 1
fib(n) = fib(n-1) + fib(n-2)
```

For reference, here are the first 10 numbers in the fibonacci sequence.

```
1, 1, 2, 3, 5, 8, 13, 21, 34, 55
```

`fibo.py`

```python
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
```
