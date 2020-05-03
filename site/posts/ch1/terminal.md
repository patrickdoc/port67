---
title: "Getting to Know Your Computer"
---

We will occasionally need to take brief detours into Computer Science history to
provide a bit of context. Computer Science moves quickly but is only around 100
years old, so we don't need to dive deep.

Let's rewind to 1980. My dad was in college and taking a class in Computer
Science. The school only had one computer, a "mainframe", that everyone had to
share. My dad's classwork had to be written down on paper and then punched out
one line at a time on punch cards.

These punch cards were an existing mechanism used in textile manufacturing.
A device called Jacquard's loom used these cards to describe complex weaving
instructions for intricate designs. Adapting punch cards for use in computers
was a natural transition.

A punch card is a stiff piece of paper with a grid drawn on it. In programming,
each column represents one letter based on which combination of holes you
punched out. Whenever you wanted to move to a new line, you simply moved to a
new punch card.

When you finished punching out your program, you stacked it on the "input" side
of the computer. The computer would then run, pulling in your cards and
processing them one by one. When it was done, it would punch out an answer on a
new card and print it out to the "output" side.

This process was tedious, full of errors, and required waiting up to a day for
your turn. It was unpleasant enough that my dad decided not to pursue
programming.

But two solutions were coming. First, engineers adapted telegraph technology so
that text could be sent from a typewriter, over a wire, to the computer instead
of using punch cards. Second, programmers added support for multiple connections
to the mainframe at the same time.

By combining these two ideas, they drastically improved the usability of
computers. Instead of waiting in line to submit punch cards, users now sat down
at one of the open "terminals" connected to the mainframe. The terminals had a
keyboard for sending instructions to the computer and a screen for displaying
the output of the instructions.

Around the same time, "graphical user interfaces", or GUIs, were being developed
for personal computers. The GUI abandoned the text-based interface of terminals
in favor of the mouse and keyboard interface that you are probably more familiar
with.

But programmers didn't abandon their terminals for GUIs. Instead they simply
built apps inside the GUIs that worked just like the terminals they were more
familiar with. So now we have the best of both worlds.

The only problem is that the average user doesn't know the terminal interface
exists. It's a powerful tool, but you are unlikely to discover it on your own.
You are even less likely to understand what to do with it once you see it.

Finding the app seems like a good place to start.

## Opening a Terminal

Modern usage of the word "terminal" generally refers to the app, instead of the
machines that were used to connect to mainframes. I will stick to that
convention.

Note: I am going to do my best to avoid machine-specific instructions. But
computer designers all put the terminal in slightly different places, so we'll
need to work to find them.

### Linux

If you are on a Linux machine, you'll have to dig around a bit. You might try
looking for an app that has `>_` in the icon. Alternatively, it will probably be
named something that is close to "terminal" or "console".

### Mac

If you are on a Mac, you are looking for an application called "Terminal". It is
buried in the `Applications` folder, so it might be easiest to just do a search
for it.

### Windows

If you are on Windows, you will have to do a little more work.

Windows is different enough from Mac/Linux that it doesn't have the terminal
built into it.  Luckily for us, they added a feature called "Windows Subsystem
for Linux", that lets you run an app that works just like a Linux computer.

Follow the official Microsoft documentation here:
[https://docs.microsoft.com/en-us/windows/wsl/install-win10](https://docs.microsoft.com/en-us/windows/wsl/install-win10)

I would recommend installing the "Debian GNU/Linux" distro from the Microsoft
Store because that will be closest to what I am using. However, you can choose
whichever you would like.

## Saying Hello

With a terminal window open, we can start with a basic introduction. First we
ask the computer its name, by typing the command `hostname` and pressing enter.

```bash
$ hostname
fearow
```

Next, we can ask the computer what it knows about us,

```bash
$ whoami
patrick
```

The output of both of these commands will likely be different for every person.

You can also ask the computer for the date,

```bash
$ date
Sun 03 May 2020 05:22:28 PM CDT
```

Examples are not terribly helpful for understanding how you can use the terminal
though. It's a bit like trying to explain how you can use a pencil by writing
down a few different words on paper.

Our main usage of the terminal is going to be to ask the computer prying
questions so that we can understand how it works. Our first line of questioning
will be about where it keeps all your data when it is turned off.
