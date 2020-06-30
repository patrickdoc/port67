---
title: "Exploring the Filesystem"
---

The first piece of your computer that we are going to work with is the
"non-volatile memory." Memory is a term for the parts of the computer that can
store data. Non-volatile means that the data isn't lost when the computer is
turned off. Non-volatile memory is responsible for holding all the data that you
expect to be there until you choose to delete it, for example, music, photos,
contacts, documents, and even preferences like which desktop background to use.

Non-volatile memory is just an idea though. To implement it, we need two pieces:
hardware that can store data without any power and software that can write data
onto the hardware and then read it back later.

Both pieces are continually evolving. When I first started using computers, the
hardware was "hard disk drives." They were a stack of magnetic disks that spun
underneath a needle. Now we have "solid state drives" that are entirely
electronic. They don't have any moving parts. This makes them faster, smaller,
and more durable.

The software is also changing, although less visibly. With hardware, you get the
satisfaction of feeling a lighter computer, and silencing the spinning disks,
and opening up your computer to transplant organs. With software, you press a
button, restart your computer, and move on with your day. It is less visible,
but the software is also changing to make your computer faster, more efficient,
and more secure.

This system is complex and constantly changing, but the software handles most of
this for you. We call this software the "file system", and it is responsible for
giving you a simple, stable interface to work with all the complexity underneath.

## A Few New Tools

Three new commands should be enough to get us started:

```
pwd - print name of current/working directory
ls  - list directory contents
cd  - change working directory
```

Go ahead and run the first two in your terminal by typing `pwd` followed by
enter/return, and then `ls` followed by enter/return.

```bash
$ pwd
/home/patrick
$ ls
Applications  Documents  Pictures
Desktop       Downloads  projects
```

Your output will likely not match mine exactly, since this will be specific to
each individual computer.

Now try `cd <directory>`, where `<directory>` is one of the names printed out by
the `ls` command.

```bash
$ ls
Applications  Documents  Pictures
Desktop       Downloads  projects
$ cd Pictures
$ pwd
/home/patrick/Pictures
$ ls
Screenshot.png
```

Again, what you see will almost certainly be different than this. You can use
just a plain `cd` to go back to the beginning.

```bash
$ cd
$ ls
Applications  Documents  Pictures
Desktop       Downloads  projects
```

As an exercise and to help you get to know your computer, I would recommend
trying to map out a bit of your file system onto paper. Here is an example of
what it might look like,

```
                           /home/patrick
                                 |
     ------------------------------------------------------
    /            |         |           |          |        \
Applications  Desktop   Documents  Downloads  Pictures  projects
                                                  |
                                            Screenshot.png
```

If you lose track of where you are or have reached a dead end, you can always
use `cd` by itself to reset to the beginning. If you try `cd Screenshot.png` and
it says `Not a directory`, you can simply mark that as a dead end.

If you've spent a fair amount of time on this computer, you may start to
recognize pieces of the file system. For example, if you are using a Mac or
Linux and `cd Desktop`, you'll find that the output of `ls` matches what you
see on your Desktop.

Similarly, you might look under `Documents` and `Downloads` and see some
familiar things. This is helpful because it means you already know about some
parts of the file system. Soon you'll have a fuller picture and will be able to
connect the pieces you know about to the new ones.
