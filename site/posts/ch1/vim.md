---
title: "Vim"
---

Our last tool for working with the filesystem is Vim. Vim is a text-editor.
Unlike Microsoft Word or Google Docs which have both text and formatting (bold,
italics, margins, columns, etc), Vim operates on "plain" text. The only data in
your file will be the words and spaces you type.

Vim is intended to help you write and edit quickly, but it has a bit of a
learning curve. When you first start, some of the quirks can slow you down.
After a while though, you'll start integrating some of the features into your
regular use and begin speeding up.

## Opening and Closing Files

Give yourself a directory to work in. Maybe something like this,

```bash
$ cd
$ mkdir projects
$ cd projects
$ mkdir vim
$ cd vim
```

Now open Vim with the command `vim`.

```bash
$ vim
```

You should be greeted by a message that starts with, `VIM - Vi IMproved` and
then includes some information about the version and the author. Most
importantly, you should have a box in the upper left corner. That is your
cursor.

To put some data in the file, type the letter `i`, followed by your name, and
then the escape key.

Note: If you hear a bell sound or see the screen flash, that means you forgot to
type `i` first. You can disable the bell/flash in the terminal settings.

Once you type something, that welcome message disappears. Which is a little bit
unfortunate because it contained the instructions on how to quit. It's not
obvious how to close Vim once you've opened it, so we'll start there.

To quit, type `:q`. Note, it should show up at the bottom of the window, not
next to your name. If it shows up next to your name, press the escape key and
try again.

You will likely see this error message appear,
```
E37: No write since last change (add ! to override)
```
Vim is warning you that you have made changes by typing your name into the file,
but have not saved them (`No write`). This gives us two options. Either we can
use `!` to override the error and discard all our changes. The command for that
would be `:q!`. Or we could "write" the changes to a file, saving them.

We are going to write our changes to a file by entering the command `:w name`.
Here `name` is the name we want to use for our new file. Now we try to quit
again with `:q`, and it should work successfully.

Your file should now be visible in the filesystem.
```bash
$ ls
name
```

We can open it again for more editing,
```bash
$ vim name
```

## Normal and Insert Mode

When you first open Vim, you are in "normal" mode. This is where all of Vim's
useful features live, but, just like the instructions for quitting, they are not
immediately obvious.

If you just reopened your `name` file after closing it, your cursor should be on
the first letter of your name. We're going to append `(<your age>)` to the end
of the line with your name.

One way would be to enter "insert" mode by typing `i`, and then holding the
right arrow key or using the mouse to move our cursor to the end of the line.
These methods work, but are slow and can stress your wrist over time.

Alternatively, you could do it the Vim way. Move your cursor to the first letter
of your name if it isn't there already and press the escape key to ensure you
are in normal mode. Then type `A (<your age>)` followed by escape. No mouse or
arrow keys needed.

Here is a slightly more complex example. Move your cursor to the end of the line
if it isn't there already. Now press `b` ("beginning") to move backwards
until your cursor is on your last name. Try `daw` ("delete a word"). It
should delete your entire last name.

Normal mode is useful, but takes time to learn. As you continue to work with
Vim, watch for patterns that you regularly follow. Watch for when you reach for
the mouse, or hold down a key and wait for the cursor to get where you want to
go. When that happens, it's usually a good bet that Vim has a faster solution
for you.

You can save and close the `name` file with `:wq`.

## Basic Normal Mode Commands

Here are some of the normal mode commands that I use most often. Many of them
come in lowercase/capital pairs, so I've listed them together. Feel free to use
all, none, or some of these.

`h/j/k/l`
:   Arrow keys. Move left / down / up / right. (If you keep
    your hands on the home row as recommended, your right pointer finger should
    rest on the `j` key, which makes these convenient.)

`i/I`
:   Enter insert mode at the cursor position / at the start of the line.

`a/A`
:   Enter insert mode after the cursor position / at the end of the line.

`o/O`
:   Enter insert mode on a new line below / above the current line.

`gg/G`
:   Move to start / end of file.

`u`
:   Undo.

`/`
:   Search a file. Press enter to submit your search, and then use
    `n` to jump to the next match.

`.`
:   Repeat previous command.

I would recommend running the command `vimtutor` in the terminal to try out some
practical examples.

## Other Options

Vim is where I spend most of my time while working. Most of what I do is
reading, writing, and editing files, so it is important for me to be comfortable
with my text editor.

If you don't like Vim, there are other text-editors available that work
differently. The primary competitor in the terminal-based editor category is
Emacs. But there are also more familiar GUI-based text editors, like Atom and
Sublime text.

I happened to learn Vim, so that's what I'll be using here. If you'd like to use
one of the other ones, you are welcome to. I would recommend giving Vim a chance
though; once you get over the initial hump, these commands all just become
muscle memory.
