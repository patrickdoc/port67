# Typeopedia

As promised, this will be an overview of common types. I would like to note
that "type" means many different things to different people. For now, I am
going to loosely define it as "a way to interpret data". Without further ado,

## Int

**Int**egers fall among the most basic types. They are the numbers ..., -2, -1,
0, 1, 2... from negative infinity to positive infinity. (That is not strictly
true, as hardware has its limitations the range is somewhere closer to -2^(-32)
to 2^(31)-1. We will learn more about this when we get to C.)

## Float

**Float**ing point numbers represent the rational numbers. They store a number
like 15239.0019234 by splitting up the number around the decimal and then
applying a few tricks to make them smaller. Floats are an impressive solution to
a difficult problem because there are infinitely more rational numbers than
integers. So how do they represent them all? Again, we will dig deeper later.

## Bool

**Bool**eans are ``True`` or ``False``. A simple way to represent this is that
0 is ``False``, and every other number is ``True``. Booleans are very useful
for controlling whether or not things happen.

```Python
if True:
    print 'yay'

if False:
    print 'nay'
```

The above program would only ``print`` "yay". We can spice things up a bit with
a special function ``==``. This function checks if the two arguments are the
same. Combined with variables, we can do something like this,

```Python
x = 1

if x == 2:
    print "x is equal to 2"

x = 2

if x == 2:
    print "x is now equal to 2"
```

which would ``print`` "x is now equal to 2". You can play around with ``==`` in
the REPL to find some interesting things. Maybe try comparing 0, 1, and 2 to
the booleans.

## List

sequence of things

## String

Strings are basically lists of characters
