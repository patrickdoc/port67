---
title: "Modifying the Filesystem"
---

While there are thousands of discoveries to make just exploring your filesystem,
we are going to move on to making your own modifications. The two basic
operations we need are creation and destruction

Note: There are far fewer safety features in the terminal compared to the
regular GUI. If you ask the terminal to delete something, it will. No questions
asked. And there is no trash can where you can recover something that you
accidentally deleted. Please think before you type.

There are two types of objects in the filesystem: files and directories (or
folders depending on who you ask). Files hold data, like pictures, songs,
essays, or apps. Directories hold files; they are just there for organization.

Returning to where we left off, let's open a terminal and run `cd` -- short for
"change directory" -- to move to our "home" directory. Your home directory and
everything inside it belongs to you. You are free to create, destroy, or edit
anything you find there.

## Directories

We don't want to accidentally delete or change anyone's important files, so
we'll start by creating a directory to play in. I'm going to call my directory
"projects", but if that name is already taken on your computer, feel free to use
something else.

```bash
$ cd
$ ls
Applications  Documents  Pictures
Desktop       Downloads
$ mkdir projects
$ ls
Applications  Documents  Pictures
Desktop       Downloads  projects
```

Following the pattern of simple command names, `mkdir` makes a new directory for
you.

Try running that `mkdir` command again exactly as you did the first time.

```bash
$ mkdir projects
mkdir: cannot create directory ‘/home/patrick/projects’: File exists
```

This time, `mkdir` fails and prints an error. It was unable to make the
directory I asked for (`/home/patrick/projects`) because it already existed
(`File exists`). If you saw this error the first time you ran the command, check
what directories already exist with `ls` and then pick a name that isn't being
used.

To remove a directory, run `rmdir`.

```bash
$ rmdir projects
```

This command fails in two common ways:

```bash
$ rmdir asdf
rmdir: failed to remove '/home/patrick/asdf': No such file or directory

$ rmdir projects
rmdir: failed to remove '/home/patrick/projects': Directory not empty
```

The first failure tells us that `rmdir` could not remove
`asdf` because there were no files or directories with
that name.

The second failure tells us that `rmdir` could not remove
`projects` because that directory still had files or other
directories inside of it. `rmdir` will only work on completely empty
directories.

## Files

Reset to your home directory and recreate the "projects" directory if you
deleted it earlier. Then `cd` into it.

```bash
$ cd
$ mkdir projects
$ cd projects
```

Now that we have a directory to work in, we can start working with files. First,
we can create them with `touch`.

```bash
$ touch bob.txt
$ touch test.html
$ touch example.pdf
$ touch 10
$ touch directory
$ ls
10  bob.txt  directory  example.pdf  test.html
```

`touch` creates an empty file with whatever name you provide. There are not many
restrictions on what you can name files, but I would recommend that you:

* avoiding using `/` and spaces
* name files descriptively

`/` and spaces can mean different things, which can cause some confusing issues.
And naming a file something like "directory" is a bit misleading. It is best to
be as clear as possible; future you will thank you.

To remove files, use `rm`.

```bash
$ rm bob.txt
$ rm test.html
$ rm example.pdf
$ rm 10
$ rm directory
$ ls
```

Again, please be careful with `rm`. There are no warnings or backups, the file
is gone forever.

## Your Toolbox

With two tweaks, you will know everything you need to know about the filesystem.

First, we can replace `rmdir` with `rm -r`. Many commands accept optional
"flags" that modify how they work. For `rm`, the `-r` flag tells it to delete
"recursively", which means directories **and** everything inside them.

Second, we need a method of writing data into files. `touch` creates files, but
they are empty. We are going to replace `touch` with `vim`, which can both
create and edit files.

Your toolbox for interacting with the filesystem will look like this,

```
# Navigating
- ls
- cd
- pwd

# Managing Directories
- mkdir
- rm -r

# Managing Files
- vim
- rm
```
