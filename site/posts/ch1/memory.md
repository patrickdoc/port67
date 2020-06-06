---
title: "Volatile Memory"
---

The final piece of your computer that we need to cover is "volatile memory". We
know from earlier that memory is a term for anything that can store data.
"Non-volatile" meant that the data was not lost when you turn off your computer.
"Volatile" means the opposite; the data *is* lost when you turn off your
computer.

It may not be immediately clear why we would want volatile memory. Choosing
between losing your data and not losing your data seems simple. However,
there is more to this choice than just volatility.

One drawback to the hard drive that we haven't seen yet is that it is slow. It's
hard to give exact numbers because the technology is constantly improving. But
if we rewind to roughly the year 2000, we can look at some approximations.[^1]

[^1]: <http://norvig.com/21-days.html#answers>

Suppose we wanted to read some data that is already stored in memory. In 2000,
most people had hard disk drives as their non-volatile memory. You could read
from these in approximately 20,000,000 nanoseconds. In comparison, volatile
memory would have only taken 250,000 nanoseconds, 80 times faster. The solid
state drives that were coming closed the gap significantly, coming in at
1,000,000 nanoseconds, only 4 times slower than volatile memory.

Rephrased in a more human scale, if reading from volatile memory takes 1 day,
then a solid state drive would take 4 days. That's not too bad, but it is
noticeable. Hard disk drives on the other hand would take 80 days. That is a
large enough difference that I would do everything I could to use volatile
memory.

## RAM

In a perfect world, we would have hardware that provides non-volatile memory and
is fast. But for now, we are stuck with two separate pieces of hardware: the
hard drive, which doesn't lose data but is slow, and RAM, which is fast but
loses data.

RAM stands for "Random Access Memory"; it is the hardware that provides volatile
memory. As the name suggests, it is exceptionally good at "random access". Hard
drives work best when you read/write data in one spot, such as in a single file.
But RAM still performs well when you read from one spot, and then write
somewhere else, and then read from another random location.

RAM is our workspace when we are programming. We have access to this great big
storage space that we can use however we would like to. Often we use it to store
intermediate results when we are working. For example, I was taught to multiply
two digit numbers in a multi-step process. It looks something like this,

```
  23
x 44
----
```

First, we multiply with the one's digit of `44`.

```
  1
  23
x 44
----
  92
```

Then we add a `0` to the second line and multiply with the ten's digit.

```
  1
  23
x 44
----
  92
 920
```

Finally, we add the two results together for the result,

```
  23
x 44
----
  92
+920
----
1012
```

When we are programming, we need some place to put `92` and `920`. They aren't
part of the result, but they need to be stored somewhere while we are
calculating the next steps. Once we have the result, the space that held `92`
and `920` can be cleared and used for something else.

### Vim

Let's look at one more example which is a bit more complex.

When you type a new letter into Vim, what happens?

One possible implementation would be to update the file on the hard drive every
time you type. The moment you press `e` in insert mode, an instruction is sent
to the CPU to add an `e` to the file, and then the CPU sends a request to the
hard drive to update the contents of the file.

This would work, but it could lead to an unpleasant user experience. You
probably don't usually notice a lag between pressing a key and seeing it appear
on the screen. But if Vim had to update the file on the hard drive every time
you added or deleted a letter, that lag might become noticeable. It might even
become so noticeable that you decide to try a different text editor.

An alternative implementation would be to keep the contents of the file in RAM.
Now when you type an `e`, instead of sending an instruction through the CPU to
the hard drive, it sends an instruction through the CPU to RAM. This makes the
user experience fast, so you don't notice any lag between typing and seeing the
results on the screen.

Our end goal is to modify the file on the hard drive, so we also need a method
to update the file with the changes we made in RAM. When you enter the `:w`
command, you are asking Vim to "write" the changes from RAM onto the hard drive.

This process of moving changes from volatile memory to non-volatile memory is
common. Although, it is more often called "saving" instead of "writing". You may
have seen warnings before that say something like,

```
Are you sure you want to quit without saving? All unsaved progress will be lost.
```

That warning message is trying to tell you that you made changes in volatile
memory but did not update the hard drive. If you quit without writing or
saving, those changes will be lost.

You may have also run into this if your computer lost power while you were
working. If the power goes out or your battery runs out in the middle of working
on something, you may find that the next time you go back your most recent
changes are missing. This is the same issue. Your changes were in volatile
memory and were lost when the power went out.

##

You imagination is the only limitation on how you use RAM. It is your workspace
to use however you would like when you are programming.

But there is a twist: RAM can only store 1's and 0's. An average computer right
now can store at least 16 billion 1's and 0's, but all that space is worthless
if we don't know how to use it.

We have one hint about how we are going to use it. We saw that the computer
converted the letter `A` to the number `65`. If we could somehow convert `65`
into 1's and 0's, we would have a way to store the letter `A` in RAM. That
wouldn't explain everything we need to know, but it would at least be a start.
