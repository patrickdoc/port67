# Vim

If you are using the same directory as last time (something like ~/Projects)
and you still have the `test.txt`, `text.png`, and `test.html`, feel free to
**r**e**m**ove them. As hinted at last time, we are going to fill a file using
Vim. So make a file with a nice name, perhaps `learnvim.txt`. (Remember
`touch`)

Vim works much like a typical bash command:

```bash
$ vim <path>
```

**V**i **IM**proved, a programmers text editor

`vim` opens the file at `<path>` for editing.

It's time to look inside a file and make some changes:

```bash
$ vim learnvim.txt
```

You should now be looking at a mostly blank screen. Along the left side, you
may have a column of `~`'s, which simply indicate empty lines. Importantly,
you should have a shaded box in the upper left corner. This is your cursor.

Perhaps surprisingly, Vim's default mode does not actually let you type. The
philosophy here is that most of the time, you are not actually inserting
characters into code or whatever else you are writing. More often than not, you
are moving around and editing. The mode you start in is then somewhat aptly
named `Normal` mode. The idea is that `Normal` mode should be your default, and
you only leave it to do something specific.

But our goal right now is to edit a file, so we want to go into `Insert` mode.
Following the convention of making shortcut somewhat mnemonic, to get to
**I**nsert mode, you just type `i`. In reaction to entering `Insert` mode, Vim
will most likely put `-- INSERT --` at the bottom of your window. This is just
a helpful reminder to let you know what mode you are in. If you type now,
things should work mostly as you expect. Let's add something now to test it
out. A standard when doing anything for the first time in computer science is
to say "Hello, world!", so I'd suggest that.

Once you've typed that, we want to save and quit. So first go back to `Normal`
mode by pressing escape (`esc` in the upper left corner of your keyboard).
Next, we want to give Vim a command, so we type `:` which should show up where
`-- INSERT --` was before. Then we use `w` to **w**rite (or save) the file, `q`
to **q**uit, and `enter`/`return` to give the command.

We should now be back at the standard terminal prompt. We can check if our
changes to the file were properly saved with a new bash command:

```bash
$ cat <file>
```

Con**cat**enate and print files.

Pretty much what it says, `cat` will print the contents of a file (or more if
you list more on the command line).

```bash
$ cat learnvim.txt
Hello, world!

```

Great, it worked! If you were to quit the terminal, restart your computer, or
even physically move your hard drive to a new computer, that file would still
be there and still say "Hello, world!". If that isn't magic, I don't know what
is.

## Some more intro Vim

While I technically taught you enough to use Vim, it would a mistake to leave
you with effectively 0 knowledge of `Normal` mode. Especially after telling
you that you should spend most of your time there! However, Vim is enormous.
It is the kind of tool that you continue learning and getting better at for
as long as you use it. All of its shortcuts and tricks were made to help, but
until you find yourself saying "I wish there were a better way to do X", you
probably won't use a shortcut to do X.

Nevertheless, I am going to introduce a couple of my most commonly used ones
here so that you start to get familiar with how they work. I also want you to
try these out as we start writing more files, so that you can practice thinking
about what you want to do before you do it.

```vim
a
```

**A**ppend, enter insert mode after the character under the cursor.

This command is very similar to `i`, it just lets you start typing after the
current character. I find myself using its sibling more often:

```vim
A
```

Capital `A` puts you into insert mode at the end of the current line. I like
to think of the this as "I **really** want to append". It's like `a`, but
stronger.

Likewise, we have a capital I:

```vim
I
```

Capital `I` puts you in insert mode at the beginning of the line. The
**really** idea doesn't apply here quite as well, but the `a`/`A` and `i`/`I`
comparison should help you remember.

```vim
h/j/k/l
```

Our last set of commands all go together. They are how you move around in
`Normal` mode. In order, they move you left/down/up/right. One neat thing
about the terminal is that it uses a "monospaced" font. This means that every
symbol that you type takes up the same amount of space. In fact, they take up
exactly as much space as your cursor. This leads to some nice looking style
choices and convenient movement. If you type some random lines of text and
then spend some time moving around it in normal mode, you'll notice that it is
very easy to tell what characters are above and below each other.
