---
title: Stack vs. Heap
...

# Stack vs. Heap

Now, at this point you may be thinking to yourself: "why do we need pointers?"
And that would be a reasonable question to ask! So far we have seen pointers
used very similarly to arrays. As we saw a little bit of last time though,
pointers give us a bit of flexibility that arrays do not.

The main advantage we saw was that we could spread out our data in memory, so
that each had room to grow. That way, we held pointers that were all the same
size, even though the strings could be any size they wanted to be. If we had
arrays, then all the data would have to be packed into one, long, continuous
chunk. And finding the second or third string would require looking for null
bytes. Instead of all that, we could just use `argv[1]` to get the second
pointer, and that string could change size without troubling us.

The second reason we need pointers is because the "memory" I have been talking
about is actually divided into sections. There are two that matter to us right
now, the "stack" and the "heap".

## The Stack

The first piece of memory is the smaller of the two. A typical size for the
stack is 8MB (that is, eight megabytes, or 8,000,000 bits). At the moment, it is
hard to imagine using up all that space, but trust me, it is possible.

The name comes from the everyday idea of a stack. For example, commonly people
use the word "stack" for a pile of papers. And that is a good image to have in
your head. Our memory stack works very much like a stack of papers. When you
declare a new variable, the computer puts that memory on the end of the stack,
just like we did when we were managing the boxes.

Let's look at some code:

```c
int main(int argc, char **argv) {
    int x = 10;
    char y = 'a';
    float z = 5.0;

    return 0;
}
```

One the three variables are declared, our stack would look like this:

    +------------+
    |     10     |
    +------------+
    |     'a'    |
    +------------+
    |     5.0    |
    +------------+

Just like trees, our stacks are upside-down. Also, it is standard to display
the heap and the stack vertically. Because I have no particular reason not to, I
am going to stick to tradition on these sorts of things.

There are two notable properties of the stack. First, just like a stack of
papers, it is very easy to add to the top of it (well, in our case the bottom).
You just drop the next item on, and it grows. Second, also like a stack of
papers, it becomes harder and harder to adjust the stack the larger it grows.
For example, if we wanted to delete `'a'` from our stack, we would have to move
backwards from `5.0`, take out the `'a'`, and then shift `5.0` into `'a'`'s
place.

Because of this, the stack is mostly for short-lived variables. For example, if
we had a function:

```c
int square() {
    int x = 3;

    return x * x;
}
```

Once that value was returned, we would really have no more reason for `x`. We
will get into the details next time, but, due to the issues deleting things from
the stack, every so often we just completely remove it and start over. If we
keep only short-lived variables on the stack, then wiping it and making a new
one should be no trouble at all.

## The Heap

The alternative to the stack is the heap. It is the larger of the two and
usually is a couple gigabytes in size.

Like the stack, the heap is named after an everyday idea. In the heap, we don't
care about data being perfectly lined up, we just care that it is there. A good
mental image might be a pile of clothes. There might not be an order to it, but
your clothes are all there. And, if you were the one that made the pile, you
might have a pretty good idea of where a particular shirt is.

Here's some hypothtical code:

```c
int main(int argc, char **argv) {
    heap int x = 3;
    heap char y = 'a';
    heap float z = 5.0;

    return 0;
}
```

In the above, I have invented a keyword `heap` that forces our values to be on
the heap. This is not a real thing, but it will illustrate the idea.

    +-------------+
    |    stack    |
    +-------------+

          ...

    +-------------+
    |     5.0     |
    +-------------+
    |     'a'     |
    +-------------+
    |             |
    +-------------+
    |             |
    +-------------+
    |             |
    +-------------+
    |      10     |
    +-------------+

In the standard picture, the stack is at the top, and grows downward, while the
heap is at the bottom and grows upward. As you can see, our heap is not as
tightly packed as our stack was. Data is spread out, and there are many blank
spaces.

This is why we need pointers! To help us find things in the stack. If we add
some addresses to our heap:

      +-------------+ 
      |    stack    | 
      +-------------+ 
                     
            ...       
                     
      +-------------+ 
    73|     5.0     | 
      +-------------+ 
    72|     'a'     | 
      +-------------+ 
      |             | 
      +-------------+ 
      |             | 
      +-------------+ 
     4|             | 
      +-------------+ 
     0|      10     | 
      +-------------+ 

We can see that this is just a vertical picture of what we had with our array of
strings last time. The nice thing about the heap is that the computer can put
data in it, like the arguments to the program, and then it just has to give us
the address so we can access it too!

Without pointers, it would be very difficult to keep track of where all our data
is. With pointers, we can use as much space as we want for long term storage.
We sacrifice a little bit of speed so that the computer doesn't have to worry
about packing the data in tightly. It is also much easier to delete from the
heap. We just tell the computer we don't want the storage anymore, and it can do
whatever it wants with it.

[Variable Scope](14-scope.html)
