---
title: Ethernet
prev: 1-networking.html
next: 3-collisions.html
...

# Ethernet

Here is the picture of our network from last time,

    +------+      +------+       +------+      +------+
    |      |      |      |       |      |      |      |
    |  A   |      |  B   |       |  C   |      |  D   |
    |      |      |      |       |      |      |      |
    +------+      +------+       +------+      +------+
       |             |              |             |
       |             |              |             |
    ---X-------------X--------------X-------------X----

We have four nodes, or computers, attached to our shared wire. We now need a set
of rules to make communication easier.

A good place to start might be to determine how to send a message to another
node. Suppose A wants to talk to C. How should C recognize that the message is
intended for it? Much like house numbers, each computer will have a unique
address. These are called "media access control addresses", or MAC addresses for
short. They are 48 bit (or 6 byte) numbers. It is standard to write each byte in
hexadecimal notation, with colons or hyphens in between each byte.

## Hexadecimal

Like binary or decimal, hexadecimal is the standard counting system with a
different base, in this case 16. What makes hex useful is that it perfectly
compresses 4 bits into a single character. Here's a chart

    decimal:    0    1    2    3    4    5    6    7
    binary:     0    1   10   11  100  101  110  111
    hex:        0    1    2    3    4    5    6    7

    decimal:    8    9   10   11   12   13   14   15
    binary:  1000 1001 1010 1011 1100 1101 1110 1111
    hex:        8    9    a    b    c    d    e    f

When hex uses up all of the digits, it moves onto letters, but only `a-f`. But,
because it is exactly 4 bits, two hex characters always make a byte. This comes
from the fact that `16 = 2^4`. Hex is a common way of writing binary data
because it is more compact and easier to read. Letters can be written in upper
or lower case.

Take for example some sample MAC addresses.
    
    E1:0E:6A:DC:8F:CC
    59:ec:5a:71:49:f5
    52:52:41:63:5E:BE

To give you a taste of what that would look like in binary, here's the first,

    1110 0001 : 0000 1110 : 0110 1010 : 1101 1100 : 1000 1111 : 1100 1100

Which is rather difficult to read.

## Using MAC Addresses

If we zoom in on a single computer, it actually looks a little more like this,

    +------+  
    |      |  
    |  A   |  
    |  _   |  
    +-|N|--+  
       -      
       |      
    ---X---

`N` is our network adapter. It is a small piece of hardware responsible for
handling all of the actual network tasks. It will transform data into
electricity and send it out on the wire, or it will decode the data and present
it to the computer.

The MAC address belongs to `N`. If `A` wants to talk to `C`, it just needs to
attach a proper address to the message.
            
    +------+      +------+
    |      |      |      |
    |  A   |      |  C   |      N:  E1:0E:6A:DC:8F:CC
    |  _   |      |  _   |      O:  59:EC:5A:71:49:F5
    +-|N|--+      +-|O|--+
       -             -    
       |             |    
    ---X-------------X---

A's message to C might look something like,

    +===================+===============+
    | 59:EC:5A:71:49:F5 |  "Hello, C!"  |
    +===================+===============+

My picture just shows the data, but we know how to translate all of that into
binary, and therefore electricity. For example, `59` is `0101 1001` and `l` is
`0110 1100`. A large amount of work has been done so that we don't have to deal
with those translations though. I'm going to mostly just write data in the form
that we understand, but remember that this translation is always happening.

When the network adapter `O` sees this message on the wire, it can simply check
the first 6 bytes. If they match `O`'s address, it will pass the message onto
`C`. If it doesn't match, `O` just ignores the message. You *can* set your
network adapter to "promiscuous" mode, where it reads every message even if the
addresses don't match. But most of the time `O` will only read the data that it
is supposed to.

If `C` wants to reply to `A`, it can just wrap it's message with `N`'s address,

    +===================+==================+
    | E1:0E:6A:DC:8F:CC |  "Greetings, A"  |
    +===================+==================+

That's all there is for simple communications. Just add an address to the
beginning of your message. Next, we will have to solve some physical problems.
In particular, what happens when messages "collide" on the wire? There is only
room for one current on the wire at a time, so the network adapters will have to
negotiate time on the wire somehow.
