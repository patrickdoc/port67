---
title: Files
...

# Files

From here on out, we are going to be making files and folders somewhat
regularly.  I am going to walk through a reasonable setup right now, but after
this, it will be up to you. You are free to organize your filesytem however you
please, to a certain extent. I would recommend creating everything as a
descendant of your home directory. As a refresher, this is the present working
directory that your terminal most likely starts in and that `cd` with no
arguments will take you to.

So let's give ourselves a nice open space to play in:

```bash
$ cd
$ mkdir Projects
$ cd Projects
$ ls
$
```

We now have a directory called `Projects` that will hold all of our files.  No
matter where you are in your filesystem, you can always get here with `cd
~/Projects`, remembering that `~` is just a special symbol for your home
directory.  If we find that we need more organization, we can make more
directories (perhaps `~/Projects/Python` to hold Python specific files).  But for
now, we should be good with a single directory.

We are going to flex our terminal muscles now to investigate how the computer
works with files. Note that we can speed the process up by passing all of the
new files we want to `touch` at the same time.

```bash
$ touch test.html test.png
```

Files can be named anything you want. However, if you follow the convention of
finishing the name with a period and an "extension", you can provide the
computer with some extra information about the contents of that file. To test
this, we can see what the operating system expects to be in each of these three
files. This is the trick that lets double-clicking to open files work, so we
are going to do the command line version of that.

On Mac:

```bash
$ open test.html
$ open test.png
```

On Linux (may not work on every computer):

```bash
$ see test.html
$ see test.png
```

Your computer most likely has a set of preferences that let you decide what the
default for each file type should be. When you open `test.html`, your browser
should open a blank page. And `open test.png` should open some photo viewing
application, possibly with an error message.

In both of these cases, the files are exactly the same. They are
completely blank, like sheets of paper. By adding the extension, we are telling
the computer what it should expect to be written in those files. As such, it
finds a proper application to read that content. Browsers are made to view
HTML, photo applications know how to read PNG's, and so on. Some of these
formats, like HTML, are completely readable by humans. They will look like
words and symbols that you could type if you wanted to. Other formats, like
PNG's, are farther along in the conversion to electricity. Often they will be
simple binary strings of 0's and 1's.

Luckily for us, programming languages are both human readable and typeable.
However, we now need a tool to edit these files. We want to write "plain text"
(i.e. words and symbols, not 0's and 1's). Interestingly, programs like Microsoft
Word and Open Office Writer won't work for us. They include things like
formatting and style in their file formats, while we need to operate on bare
"plain text".

There are many apps that *do* let us edit files the way we need to, but we are
going to stick to terminal programs for the time being. There are two modern
powerhouses in the terminal text editing game: Vim and Emacs. People have been
fighting over which one is better since the dawn of time and will most likely
continue to do so forever. In the end, the decision is pretty much meaningless,
and just depends on who teaches you. In this case, that is me, and I am a Vim
guy. You are certainly welcome to use any editor you want (so long as it doesn't
include extra stuff like Word or Open Office), but I would recommend Vim for
now. Eventually, you will be able to choose an editor for yourself, maybe even
branching into the graphical ones like Atom or Sublime.

One of the better qualities of Vim is that it comes installed on most systems.
And when it is not, it is highly likely and older version called Vi is. (Vim
comes from "Vi Improved"). Vim is going to take a full lesson to get started
with, so I'll end this here. In fact, one of the most Google'd questions about
programming is, "how do I exit Vim?" So I'll wait until next time to suggest
opening it.

[Vim](5-vim.html)
