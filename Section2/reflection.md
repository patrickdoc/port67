---
title: A Reflection
section: Section2
...

# Reflection on C

We've covered an enormous amount of material in this section. I certainly don't
expect that it you have a perfect understanding of it all yet, but you should be
at least thinking about the right things. With the right picture in your head
about what is happening behind the scenes, programming becomes much easier. If
you try to print a string and get garbage, you should immediately think "I
forgot the final null byte."

At this point, I really haven't given you much of an opportunity to practice.
But that is about to change. We have covered enough that I can start pushing
more of the work of writing programs onto you, and I will be filling in the
extra libraries and functions that you need. Using pointers is just part of
working with C, and I think it is best to help you understand what they are and
how they work before I set you loose with them.

To really gain mastery with programs and C, the only option is to practice. But
writing programs that print out the fibonacci sequence or sum up grocery bills
gets boring quickly, so I am going to walk you through real problems. That way
you can learn to code while also learning about the rest of the world of
computer science.

## Networking

One of your options for the next track to follow is networking. This is how
computers communicate with each other. The biggest computer network is the
Internet, but it isn't the only one. Businesses often connect big groups of
computers to all work together. Companies like Amazon need hundreds to run their
website and store all their data.

My goal for this section was to give you enough information that we can start
talking about bigger topics without being bogged down by programming details.
For instance, the most basic networking code works like this,

```c
char *incoming_buffer = // SOME LOWER LEVEL THING;

printf("The first byte from the network is: %c\n", incoming_buffer[0]);
```

Obviously I am glossing over some major details but that's about it. You should
be able to look at that and have a reasonable estimate of how it works. "Buffer"
is a term used for a space in memory, often an array, that holds temporary data,
in this case data from the network. We ask the computer for the contents of that
buffer in the first line, and then print some of it out in the next. That's all
there is to network programming.

The real trick is then doing interesting things with that data and sending
replies back to whoever sent stuff to you. A great example of this is a chat
server. A bunch of people log into the server and then send their messages to
it. The server then accepts those messages and forwards them to the clients who
print it out for the user to see. We are just shuffling data around between
computers.

Without this section, we would have needed to do a bit of work to understand
even those two lines of code above. But the good news is that this information
applies to basically any domain that you could write a program for. Now that we
have this foundation, we can go on to talk about all sorts of fun things.

## Operating Systems

If instead you would like to move down the abstraction stack, you can learn
about operating systems (OS's). The most popular OS's are Linux, MacOS, and
Windows. They are just about the lowest level of software running on your
computer. They are what make your computer work.

The OS provides an abstraction of all the hardware in your computer case so that
C can use the different pieces without knowing exactly what they are. I can buy
a hard drive from any manufacturer, and C doesn't care. It just calls
"write(data, file)", and the OS handles getting that data into the file.

(filesystem, parallelism, concurrency, ram, HDD/SSD, cpu)

## More about your computer

The route with the least programming, this is about getting familiar with using
your computer and the new functionality that you can find on it.

154???

(installing packages, config files, bash, python, vimrc, bashrc)
