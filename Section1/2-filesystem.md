---
title: The Filesystem
...

# The Filesystem

All of the information on your computer needs to be kept somewhere. When you
save something you are working on, download a new song, or even download an app,
you are creating blocks of information on your computer called files. A file
roughly corresponds to a single song or image or video and so on. The trick is
keeping all of these files organized so that both you and your computer know
where they are. We call this organization the filesystem. You may have some
experience with it, using Finder on Mac or Konqueror, Nautilus, or Thunar on
Linux.  These are graphical interfaces, but we are going to use the terminal for
a little more power.

Just like last time, we need to open up our terminal application. I am going to
introduce a few new commands that let us interact with the filesystem. A good
place to start is figuring out where we are:

```bash
$ pwd
```

`pwd` shows us the **p**resent **w**orking **d**irectory. My result looks like
this:

```bash
/Users/patrick
```

If all of your files were all together in one big pile, it would be difficult
to find and organize your computer. Luckily, the filesystem has a way of
helping you sort everything. In addition to files, your filesystem has
directories (also known as folders). These directories can hold files and even
more folders to help you keep track of everything. You may already be familiar
with your Documents or Downloads folders, but your computer actually has
thousands.

These earliest lessons serve a dual purpose. The aim is to introduce a number
of necessary tools before we jump in, but also to become familiar with some of
the recurring themes. As such, we are going to take a minute to talk about
trees.

       |
      / \
     /\ /\
    o  oo o


Computer science trees are almost all bark and almost always upside-down.
(Caption)

We use the term tree as "something with branches and leaves." So let's consider
the filesystem again. In this case, it might look like:

                      Home
                    /      \
            Documents       Downloads
            /               /        \
    resume.doc          song.mp3    picture.png

With a bit of imagination, it is possible to see how this could be a tree. You
have a root or main trunk called `Home`, two branches called `Documents` and
`Downloads`, and three leaves coming off of those branches. I know it is a bit
of a stretch, but get used to it because they are everywhere. There are a couple
details to notice here:

## Paths

Our tree structure defines a unique path from wherever you are to any
file. Suppose we wanted to know where `song.mp3` was. Looking at the picture
above, we can say:

* Start at the root of the tree (`Home`)
* Move down to `Downloads`
* Move down to `song.mp3`

These paths come up often, so we have a compact way of writing it. In this case,
it would be `Home/Downloads/song.mp3`. That is the absolute (or "from the root")
path to `song.mp3`.

We also have relative paths. These describe how to get somewhere from where we
are already. So let's pretend we are sitting at the Downloads folder. We have
two special names that we use in relative paths. The first is `.`, which means
"the current directory." In our case, `.` is `Downloads`. So to get to
`song.mp3`, we just have to say:

* Start where we are (`Downloads`)
* Move down to `song.mp3`

Which translates nicely to `./song.mp3`. We can do even better in this specific
example by just using `song.mp3` because the computer already knows where we
are. It's good to know how `.` works though.

Our second special name is `..`, which means the parent of where we are. The
directions to `resume.doc` would be:

* Move up to our parent (`Home`)
* Move down to `Documents`
* Move down to `resume.doc`

Translated, we have `../Documents/resume.doc`. Paths are a simple but powerful
way of describing how to move around the filesystem, and we will see how to
exploit them shortly.

## Trees are Recursive

If we cut off any branch and just examine it, we can see that it is still a
tree. We even include a single leaf in our definition of trees. This property
leads to a simple design. A nice example of this is moving entire chunks of the
tree. For example, let's make `Downloads` a child of `Documents`. We cut off the
connection between `Home` and `Downloads` and recreate it under `Documents`:

                     Home
                    /
            Documents
            /       \
    resume.doc       Downloads
                     /        \
                 song.mp3    picture.png

Our tree is now a little lopsided, but that is fine. We just moved all of those
files by changing one little line. This is a major win for us. Also relative
paths...

## Files Can't Have Children

In the tree jargon, files are leaves. In the same way that real leaves can't
have branches coming out of them, files can't have directories or other files
below them. Files exist to hold data, while directories exist to keep things in
order.

## Directories are Infinite

To complement files not having any children, directories can have as many as
you want. (Probably like 2^32 or something...) Expanding on the example above,
you could have:

                                 Home
         _________________________|______________________________
        /               /            \             \             \
    Documents      Downloads      Pictures      TODO.txt      Recipes

Here we have four child directories and a file. But you could have however many
make sense for you. You might be someone who takes pictures all day, in which
case you could have thousands of pictures in your Pictures folder. Or you might
have a set of directories within Pictures for Family, Vacation, Work, etc.
which help you keep track of all your photos.

## Basic Commands

Now that we know all about paths and the tree structure, let's see what we can
do. Assume our present working directory is Home, and we are working from the
tree that has Downloads as a child of Documents.

```bash
$ ls <path>
```

**L**i**s**t the contents of the given directory at <path>. With no argument,
list the contents of the present working directory.

Possibly the most useful command, `ls` shows you what files and folders are
around you. If we want to know what is in Documents, we could do `ls
Documents`, returning `resume.doc Downloads`. We could even look inside
Downloads with `ls Documents/Downloads`, but this quickly gets out of hand.

```bash
$ cd <path>
```

**C**hange **d**irectory. With no argument, move to the user's home directory.

`cd` is the solution to our issue above. Instead of referencing
`Documents/Downloads/song.mp3`, we can first move: `cd Documents/Downloads`,
and then just use `song.mp3`. Try moving somewhere for yourself and then `pwd`.
You can also use a relative path here, so `cd ../..` would move us to the
parent of our parent. In our case, it would move us from
`Home/Documents/Downloads` back up to `Home`. Just `cd` by itself should always
take you back to the start.

```bash
$ touch <path>
```

Change file access and modification times.

`touch` does not have a neat mnemonic, and one of its most common uses is not
even listed in the description. We will get to file metadata like access and
modification times eventually, but for now we will focus on another use. `touch`
creates an empty file at the path you give it if there is nothing there already.
This is the quickest way to create a file.

```bash
$ mkdir <path>
```

**M**a**k**e **dir**ectories.

As simple as they come, this command creates a directory at the given path
location.

```bash
$ rm <path>
```

**R**e**m**ove files.

This is our first command that has a real chance of causing damage. Sometimes
it is smart enough to double check that you really want to delete some
important file, but most of the time it will just erase it forever. This is not
intended as a scare tactic, I just want to make it clear that we are taking the
training wheels off. It is up to you to ask yourself "Am I sure?" before using
`rm`.

```bash
$ rmdir <path>
```

**R**e**m**ove **dir**ectories.

Almost as easy as `mkdir`, `rmdir` has the one "gotcha" that the directory must
be empty for you to delete it. You can do this by deleting all the files within
first.

```bash
$ mv <source> <destination>
```

**M**o**v**e files.

This moves the file at the `<source>` path to `<destination>`. As an example,
(with Home as our `pwd`) `mv Documents/Downloads Downloads`. This command will
move the `Downloads` directory (and all of its children) back to the `Home`
directory, like in the original tree. Equivalently, we could also do:

* `mv Documents/Downloads ./Downloads` (using a relative path)
* `mv Documents/Downloads ./` (move the source to the `pwd` keeping the same
  name)

This brings up a second nifty use of `mv`. We can rename files. Suppose we were
going to make a new resume and want to use `resume.doc` as the name. We can't
have two files with the same name in the same directory (try writing the paths
for both), so we have to change the name of the old one. We can do that with
`mv Documents/resume.doc Documents/oldresume.doc`.

```bash
$ cp <source> <destination>
```

**C**o**p**y files.

This is almost identical to `mv`, but it doesn't change the original file.
Using the example above, `cp Documents/resume.doc Documents/oldresume.doc`
results in both `resume.doc` and `oldresume.doc` existing within `Documents`.

```bash
$ man <command>
```

Format and display the on-line **man**ual pages.

This is how you learn more about commands or check how they work. If you try
`man man` you'll see that I shamelessly copied some of my descriptions from the
man pages for the above commands. `man` should always be your first stop if you
have any questions about commands.

Once you are in a man page, you can move up and down with "j" and "k", and quit
with "q". This actually opens a program called "less" (a joke on the earlier
program "more"...). Also this style of movement will come up again, so keep it
in mind.

[Python](3-python.html)
