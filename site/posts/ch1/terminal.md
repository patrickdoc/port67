---
title: "The Terminal"
---

To begin our investigation into computers, we must first take a step
backwards. Before the modern visual interfaces with buttons and scrollbars and
pop-up windows, users were given a simple text-based interface. Instead of
having a clock displayed on your screen, you had to ask.

```bash
$ date
```

And in reply, you were given,

```bash
Sat 01 Feb 2020 01:00:56 AM CST
```

The benefits of our more developed system are obvious; we can simply look to one
edge or corner of our screen whenever we please and find the time there.

The benefits of the older, text-based system are less obvious. One compelling
reason to favor the old over the new is that it is consistent across platforms.
If I ask my computer for the `date`, and you ask your's, they will work the same
way. There might be small differences, but the general functionality will match.

We are going to be spending the majority of our time working with the text-based
interface, so the natural first step is learning how to access it.

## Opening a Terminal

The quickest way to begin working with the text interface is through apps that
are often called "terminals", "command lines", or "consoles".

Once you get up and running with a working terminal, we should __hopefully__ be
able to avoid future platform-specific instructions. But for now, we'll have to
accept the differences.

### Mac

If you are on a Mac, you are looking for an application called "Terminal". It is
buried in the `Applications` folder, so it might be easiest to just do a search
for it.

### Linux

If you are on a Linux machine, you'll have to dig around a bit. You might try
looking for an app that has `>_` in the icon. Alternatively, it will probably be
named something that is close to "terminal" or "console".

### Windows

If you are on Windows, you will have to do a little more work. Windows is
different enough from Mac/Linux that it doesn't have the terminal built into it.

Luckily for us, they added a new feature called "Windows Subsystem for Linux".
This lets you run an app that works just like a Linux computer.

Follow the official Microsoft documentation here:
[https://docs.microsoft.com/en-us/windows/wsl/install-win10](https://docs.microsoft.com/en-us/windows/wsl/install-win10)

I would recommend installing the "Debian GNU/Linux" distro from the Microsoft
Store, since that will be closest to what I am using. However, you can choose
whichever you would like.

## Running Your First Command

With a terminal window in front of you, try out the `date` command from earlier.
Simply type "date", and press the Enter/Return key.

```bash
$ date
Sat 01 Feb 2020 01:00:56 AM CST
```

Congratulations! While `date` isn't the most exciting command, you've just
unlocked a new way to talk to __any__ computer.
