---
title: The Terminal
section: Section1
next: 2-filesystem.html
...

# The Terminal

## A Bit of History

It is convenient to think about computers as a large stack of ideas. At the top
is the user and at the bottom is electricity running through the wires of the
computer. In between are layers of interfaces and abstractions. These layers all
have to work together to turn your swipe or key press into something that the
computer understands. Somehow, pressing a key needs to control the individual
electrons flowing through the computer to tell it what to do. It is not
surprising then, that many of the great names in the development of computer
science were educated as electrical engineers. They had to create many of the
lowest layers of the stack before mathematicians could come in and start
figuring out what to do with this new technology.

This stack will form something of an outline for learning about computers.
While they were developed from the bottom upward, at this point it makes more
sense to work from the top down (at least for the time being). These devices
are so prevalent in society that almost everyone knows how to use them, but a
relatively small number of people know how they work. This is part of the
reason that it makes sense to start at the top; everyone already knows about it.

So to start, we will take our first step down a layer. We will move from the
graphical (or image-based) interfaces to a textual one. Find your terminal or
console application. On Macs, the default is Terminal. On Linuxes, try to sniff
out something that sounds like "terminal" or "command". Computer scientists have
a particular humor about naming things. A somewhat representative example is a
terminal app called Terminator. While I have found these to grow stale after a
while, they do often prove helpful in identifying what a program does. So keep
an eye out.

## Using the Terminal

Now that you have a terminal window open, we should orient ourselves. The
window should be almost empty, with the exception of a single line. What is
displayed here is highly configurable and so will look different in almost
every environment. Most likely though, you will see some sort of final character
(most often a `$`) followed by a possibly blinking box. This box is your cursor,
and it is waiting for you to type.

The interface that we will soon begin playing with is ubiquitous in computer
science. You type some input, the program reads the input and reacts to it, and
then it gives you a prompt to do something else. Try pressing return/enter a few
times. Or try some random letters and then return/enter. 

A word of caution: As we start stepping down through the layers, we are
simultaneously gaining power and losing restraints. We are now in the "do what
I say, not what I mean" realm and can cause actual problems. I will do my best
to warn you about tricky spots, but the best advice I can give you is to think
before you hit enter. The computer won't ask you if you are sure, so you have
to add that step in for yourself.

When you just hit enter by itself, `bash` (the program you are running) gladly
does nothing and moves on to the next prompt. If you just typed some nonsense
letters, than `bash` probably responded with:

```bash
-bash: asdf: command not found
```

This means that `bash` expected you to give it a real command, but it was unable
to find a command with that name. So let's try a real command:

```bash
$ date
Tue Jul 11 16:59:52 CDT 2017
```

`date` is one of a large number of built-in commands that come standard on most
computers. There is a nice bunch of commands to go through next, but we'll need
to learn a bit about the filesystem first.
