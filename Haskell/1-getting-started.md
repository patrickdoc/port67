---
title: Getting Started with Haskell
prev:
next:
description:
...

# Installing Haskell

While C and Python are so widespread that they come pre-installed on most
computers, Haskell must be downloaded and installed. How you do this varies
based upon what kind of computer you are using. So I've broken it down into a
couple groups. If you can't find one that works for you, feel free to contact me
and I'll help!

We will be installing the Haskell Platform. There are two components to this
first, `ghc`, the Glasgow Haskell Compiler. Like `gcc` or `clang`, `ghc` is a
compiler. But instead of reading C and outputting executables, it reads Haskell
and outputs executables. The second component is a variety of commonly used
libraries and tools.

The Haskell Platform aims to provide a "batteries included" feeling. For
comparison, Python also tries to give you everything you need for most tasks. On
the other side of the spectrum, we have C, which gives you next to nothing. I
specifically chose C and Python because they are opposites in many respects. As
you learn about more languages, you'll find that the differences are less clear.

If you feel confident, this is essentially a longer form of the page
[here](https://www.haskell.org/platform/).

## Mac

If you are on a Mac, I would recommend getting [Homebrew](https://brew.sh/). It tries to be a
Linux-style package manager for Macs, and I've found it to work quite well.

To install Homebrew,

```bash
$ /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
```

!!! FIXME: brew does ask for sudo :/

Note: I've said it before, and I'll say it again: be careful with blindly copy
and pasting commands at the prompt. This line downloads and runs a Ruby program
to install Homebrew. It does not ask for root access with `sudo`, which is a
good sign. Still, be wary of what you see on the internet.

Once you have `brew`, you should run,

```bash
$ brew update
$ brew upgrade
$ brew cask install haskell-platform
```

## Linux

If you are on Linux, the official page provides good instructions:
[](https://www.haskell.org/platform/#linux).

Depending on which distribution of Linux you are using you will be using
different package managers. However, they all work roughly the same way . I'm
going to us `apt-get` as the example because it is probably the most popular.

It is always a good idea to update your package manager before installing new
software.

```bash
$ sudo apt-get update
$ sudo apt-get upgrade
```

These two steps lookup new changes to the software on your computer and then
install them. You may have used computers or phones that tell you it is time to
upgrade before. Linux trusts you to do these yourself. Keeping your computer up
to date is usually a good idea, so I'm going to recommend updating regularly.

After that you simply install the platform. Most distributions have it
available.

```bash
$ sudo apt-get install haskell-platform
```

## Windows

The Windows instructions on the official page are short and sweet:
[](https://www.haskell.org/platform/#windows).

I would recommend selecting the "Full" version of the platform. If you are tight
on space, go ahead and choose "Core"; you just might need to install a couple
packages later.

# Run GHCi

To check that everything went smoothly, try running the interpreter:

```bash
$ ghci
GHCi, version 8.2.1: http://www.haskell.org/ghc/  :? for help
Prelude>
```

If something went wrong, try to debug it! This is a learning experience! If you
still can't get it, feel free to contact me. Installing any new software can go
wrong for 100 different reasons, and I don't want you getting discouraged before
we even start.
