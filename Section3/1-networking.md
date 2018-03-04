---
title: Networking
next: 2-ethernet.html
...

# What is Networking?

We now have a fair bit of experience talking to computers. We write code and
then tell the computer how to read it. We have some translators along the way,
like Python and C, but mostly we know how to get the computer to do what we want
it to.

While working on a single computer is useful, a lot of the time, you are
actually interested in communicating with other computers. When you are browsing
the internet, you are actually sending out requests to other computers. You call
out for the home page of something.com, and, most of the time, that specific
file is found on a computer somewhere in the world. That computer then sends the
contents of the file back to you, and the browser displays it.

Streaming songs or videos works the exact same way. The name "streaming" comes
from the fact that there is a literal stream of data starting at Netflix or
Spotify and flowing all the way to you. This stream contains all the information
needed to play the audio or video that you requested.

By the end of this section, you should be able to write your own networking
code and understand how the internet works from the computer's perspective.

## A Bit of History

The first tool we need is a way to connect computers. If computers run on
electricity, then all we need is a wire.

We could theoretically network two computers together something like this,

    +------+      +------+
    |      |      |      |
    |  A   |------|  B   |
    |      |      |      |
    +------+      +------+

Where the wire between them allows for communication. However, we would actually
need a pair of wires so that the computers could send messages at the same time.
Like water, electricity can only flow in one direction at a time. But this idea
does not work well for any more than two. Look at what happens when you want to
do four.

    +------+       +------+
    |      |       |      |
    |  A   |-------|  B   |
    |      |       |      |
    +------+       +------+
       |    \     /   |
       |     \   /    |
       |       X      |
       |     /   \    |
       |    /     \   |
    +------+       +------+
    |      |       |      |
    |  C   |-------|  D   |
    |      |       |      |
    +------+       +------+

Already we need 6 pairs of wires to allow all of them to communicate.

In the 1970's, the famed Xerox Palo Alto Research Center (PARC) created a
solution called Ethernet. It specified a way to greatly simplify networking.
Here's what it looked like,

    +------+      +------+       +------+      +------+
    |      |      |      |       |      |      |      |
    |  A   |      |  B   |       |  C   |      |  D   |
    |      |      |      |       |      |      |      |
    +------+      +------+       +------+      +------+
       |             |              |             |
       |             |              |             |
    ---X-------------X--------------X-------------X----

Now each computer clamps onto a single long wire. The `X`'s literally chomped
through the plastic and metal shielding on the coaxial cable to make a
connection with the inner wire. This system has 3 important features that are
often summed up as Carrier Sense, Multiple Access with Collision Detection.

Carrier Sense means that the computer can tell when the network is busy. It
knows when the wire is in use, so it can avoid interfering with other traffic.

Multiple Access means that many different computers can all connect to the same
network. In the first attempt with 6 wire pairs, we had no way to share any of
the connections. This system lets the computers all use the same wire.

Finally, Collision Detection lets a sender know when its message runs into
another along the way. Here's an example to see how that might happen,

                     Network is idle
    +------+      +------+       +------+      +------+
    |      |      |      |       |      |      |      |
    |  A   |      |  B   |       |  C   |      |  D   |
    |      |      |      |       |      |      |      |
    +------+      +------+       +------+      +------+
       |             |              |             |
       |             |              |             |
    ---X-------------X--------------X-------------X----

                A sends to C, D sends to B
    +------+      +------+       +------+      +------+
    |      |      |      |       |      |      |      |
    |  A   |      |  B   |       |  C   |      |  D   |
    |      |      |      |       |      |      |      |
    +------+      +------+       +------+      +------+
       |             |              |             |
       |             |              |             |
    ---X-----O>------X--------------X----<O-------X----

                Messages collide and are lost
    +------+      +------+       +------+      +------+
    |      |      |      |       |      |      |      |
    |  A   |      |  B   |       |  C   |      |  D   |
    |      |      |      |       |      |      |      |
    +------+      +------+       +------+      +------+
       |             |              |             |
       |             |              |             |
    ---X-------------X------OO------X-------------X----

Because of Collision Detection, A and D now both know that their messages did
not reach their targets.

With a solid physical infrastructure in our hands, we can discuss the ways in
which the computers agree to use the network. This is called the "protocol", and
it defines how things must react to different events. For example, how should A
and D respond when they find out their transmissions collided?
