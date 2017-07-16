# The Filesystem

All of the information on your computer needs to be kept somewhere. When you
save something you are working on, download a new song, or even download an
app, you are creating blocks of information on your computer called files. A
file roughly corresponds to a single song or image or video and so on. The
trick is keeping all of these files organized so that both you and your
computer know where they are. We call this organization the filesystem. You may
have some experience with it, using Finder on Mac or Konqueror/Nautilus/Thunar
on Linux.  These are graphical interfaces, but we are going to use the terminal
for a little more power.

Just like last time, we are going to open up our terminal application. I am
going to introduce a few new commands that let us interact with the filesystem.
A good place to start is figuring out where we are:

```bash
$ pwd
```
`pwd` shows us the *p*resent *w*orking *d*irectory. My result looks like this:

```bash
/Users/patrick
```
If all of your files were all together in one big pile, it would be difficult
to find and organize your computer. Luckily, the filesystem has a way of
helping you sort everything. In addition to files, your filesystem has
directories (also known as folders). These directories can hold files and even
more folders to help you keep track of everything. You may already be familiar
with your Documents or Downloads folders, but your computer actually has thousands.

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
have a root or main trunk called Home, two branches called Documents and
Downloads, and three leaves coming off of those branches. I know it is a bit of
a stretch, but get used to it because they are everywhere. There are a couple
details to notice here. (And if you don't get all of them now, that's fine. You'll learn more as we keep going).

- Every branch is another tree
- Files don't have children
- Directories can hold any number of things
- Paths



Now that we know where we are in the tree, let's explore what we can do:

```bash
$ ls
```

*L*i*s*t the contents of this directory (optionally, of the directory given)


```bash
$ touch
```

Update the access time of a file if it exists, otherwise create an empty file

- rm
- mkdir
- rmdir
- cd
- mv
- cp
- man
