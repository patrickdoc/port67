---
title: Memory
...

# Memory

When you ask the computer to store a value, like `x = 10;`, it needs to find
a place to put it. As we know from the last lesson, we can store this number in
binary. It would look like `1010`. So all we need is something that can store
binary. Although there are many pieces of hardware in your computer that can
accomplish this, we group them all under the concept of "memory". If we say that
a value is "in memory", that means the computer knows where it is and can get it
for us. However, it is up to the computer to store it as it sees fit.

We are going to play the role of the computer managing memory to get a feel of
how the pieces fit together. For simplicities sake, let's manage a single byte
of memory. That means we need 8 boxes that can each hold 1 bit.

                   One Byte
    +---+---+---+---+   +---+---+---+---+
    |   |   |   |   |   |   |   |   |   |
    +---+---+---+---+   +---+---+---+---+

Alright, we are now in charge of these 8 boxes. I've split them up into groups
of 4 to make it a little easier on the eyes and keep with the traditional
formatting of a byte.

Our first task is to store the number 10. As mentioned above, we can write this
as `1010`. Let's put it in our boxes.

                                x
    +---+---+---+---+   +---+---+---+---+
    |   |   |   |   |   | 1 | 0 | 1 | 0 |
    +---+---+---+---+   +---+---+---+---+

We now get two more requests: `y = 0;` and `z = 6;`. We are tight on
space, but we can just fit them both.


    |     z     | y |   |       x       |
    +---+---+---+---+   +---+---+---+---+
    | 1 | 1 | 0 | 0 |   | 1 | 0 | 1 | 0 |
    +---+---+---+---+   +---+---+---+---+

Hopefully my art is clear. We are storing these numbers right next to each
other, but keeping track of which bits belong to which. If these were real
boxes, we could slide them all apart and label them to make it clear how they
were meant to be grouped. But I want to be sure you understand that "memory" is
really just an astounding number of these boxes all lined up next to each other.

## Running Out of Space

Now we run into a problem. `x = 24`. In binary that would be `1 1000`. I've
added the space to help read the value, but dropped the usual 0's on the left
because we don't technically need them. But even saving space like that doesn't
help us. `x` now needs 5 bits, but we don't have any room for another bit.

How do we solve this problem? We might consider kicking `y` out. In that case we
could do this:

    |     z     |           x           |
    +---+---+---+---+   +---+---+---+---+
    | 1 | 1 | 0 | 1 |   | 1 | 0 | 1 | 0 |
    +---+---+---+---+   +---+---+---+---+

But if the program comes and asks for the value of `y`, what are we supposed to
do? We haven't lived up to our side of the deal. `y` is no longer in our storage
because we decided to get rid of it. So this solution probably wouldn't work.

Alternatively, we could just tell the program "no" when it tries to update the
value of `x`. This solution is good for us as memory management. We don't have
to kick out any values, and we don't even have to do the work to update `x`. But
the programmer probably won't be happy when her program doesn't work. So what do
we do?

Like all solutions, we need to compromise. So we need to be able to store
variables compactly, while also leaving enough room for them to grow. Now, it
would be impossible to accomodate all values; a number like 2^10 would require
11 bits, and we only have 8!

We can help the issue by giving the programmer a specific number of bits. For
example, we could tell the programmer that she only gets 4 bits. Then when she
tries to update `x = 24`, the bug would be her fault. We promised her 4 bits,
but she tried to use 5.

Our programmer would also have another issue. Under our new rules, every
variable gets 4 bits. This means our memory looks like this:

    |       y       |   |       x       |
    +---+---+---+---+   +---+---+---+---+
    | 0 | 0 | 0 | 1 |   | 1 | 0 | 1 | 0 |
    +---+---+---+---+   +---+---+---+---+

There's no more room for `z`! It seems like our "solution" has only caused more
problems! However, our rules are actually helpful. Imagine if instead of causing
the problem by updating `x = 24`, our programmer had instead asked us for `y =
2`.  Surely we should support a number being set to 2. But this would have
caused a conflict with `z`, and we would have come to the same conclusion.

## Providing Size Options

We have almost solved our problem, but we have one more issue to handle.  If we
check the range of 4 bits (`0000` to `1111`) we will see that our variables can
only hold values up to 15. What if a program needs a value bigger than 15? We
need some way to specify the range of values we need.

At long last, we have come to the first purpose of types (there are two that
matter to us right now). Types let us specify the range of values that our
variables can hold. Using our example, let's define the types `small` and
`large`. When a program says `small x = 10;`, we fill in memory like this:

                        |       x       |
    +---+---+---+---+   +---+---+---+---+
    |   |   |   |   |   | 1 | 0 | 1 | 0 |
    +---+---+---+---+   +---+---+---+---+

If they try to set `small x = 24`, we send them an error that says they only get
4 bits.

If they need to store numbers between 0 and 255 (`0000 0000` to `1111 1111`),
they can ask for `large x = 24`. In which case we set aside 8 bits for them.

    |                 x                 |
    +---+---+---+---+   +---+---+---+---+
    | 0 | 0 | 0 | 1 |   | 1 | 0 | 0 | 0 |
    +---+---+---+---+   +---+---+---+---+

If they need numbers bigger than that, they have to buy us some more boxes!

# A Challenge

There's one more trick to types. It answers the question, how do you store -1?
I would encourage you to think about this problem for a bit. If you were to
create a type for negative numbers, what would the range be? How many bits would
you need? The range you choose will define how many bits you use. If you wanted
to store -1 to 1 (3 different values) you would need 2 bits. 1 bit can only
store 2 values. But if you were going to store -1 to 1, you would have some
pattern of bits that was unused. With two bits, you have `00`, `01`, `10`, `11`.
So try to use all your bits!

Our usual rule of summing all the bits multiplied by powers of two won't work
because that number will always be positive. You will have to come up with a way
to indicate a number is negative. Also note that we will probably want to be
able to store both positive and negative numbers in the same type.

# Next
[Interpreting Memory](8-interpreting-memory.html)
