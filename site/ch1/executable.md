---
title: "Executables"
---

Hopefully you are now comfortable using some of the commands we've covered so
far. Commands like `ls` and `cd` will be the base that we build the rest of our
knowledge on.

To start building the next level, we need to figure out how these commands work.
We need to answer the question, what is `ls`?

We can make an educated guess based on what we know. First, `ls` does not
disappear when you turn off your computer. Every time you open a new terminal
window, you can run `ls`. So `ls` must be kept in non-volatile memory somewhere.
Second, the only non-volatile memory we know about is the hard drive and the
filesystem. So `ls` must be somewhere in the filesystem.

To prove our theory we just need to find `ls`. You are welcome to go looking for
it, although it can be a bit hard to find if you aren't familiar with some of
the conventions. If you are going to look for it, I would recommend starting in
the root directory `/`. If you would rather skip straight to the result, try
this,

```bash
$ which ls
```

`which` tells you where a command is in the filesystem. If you go to the
directory `ls` is in and run `ls`, you might see some other familiar commands.

```bash
$ which ls
/bin/ls
$ cd /bin
$ ls
...
ls
...
pwd
...
rm
rmdir
...
```

All of these commands are just files. And like any other file, we can try
opening it with Vim.

```bash
$ vim /bin/ls
```

Most of the file will be unreadable because the data inside is for the computer
to read, not humans. But, there will be little bits that are readable. Let's try
opening Vim with Vim.

```bash
$ which vim
/usr/bin/vim
$ cd /usr/bin
$ vim vim
```

The file is just as unreadable as before, but try searching for something like
your name. Make sure you are in normal mode by pressing escape, then type
`/Patrick` and press enter. When I do that, I get a little error in the bottom
corner,

```
E486: Pattern not found: Patrick
```

Now try searching for `E486:`. In the middle of all the garbage, you'll find
that exact warning phrase `E486: Pattern not found: %s`. So this file means
something, just not to us.

## Executable Files

These files are "executable", which means that they are a set of instructions
that the computer knows how to execute. When programming, our goal is to create
these files so that we can ask the computer to execute them for us.

But if we can't even read these executables, how are we supposed to write our
own?

Computer Science has a standard solution to this problem. When you are working
with something that is complex and constantly evolving, you create a simple,
stable interface, and then write software to translate for you.

We've seen this once already in non-volatile memory. We had constantly evolving
hardware like hard disk drives and solid state drives, we have a stable set of
commands like `ls`, `cd`, and `mkdir`, and then we have the filesystem which
translates our commands into changes on the hardware.

Once again, we are going to see the same solution. This time, we have to handle
the complexity of the central processing unit, or CPU. Our interface will be a
programming language. And the software that will translate is called a
"compiler".
