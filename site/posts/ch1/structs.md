---
title: "Composite Types"
---

You could theoretically write any program you wanted with just the primitive
data types. But you'll eventually run into an organizational issue. Most data is
not just one number or one letter. We need a way to express a type that is
composed of multiple smaller types.

C allows us to create our own "structures" that hold multiple pieces of data.
Here is a simple structure that holds weather data.

```c
struct Weather {
    int temp;
    float humidity;
}
```

This defines a structure called `Weather` that holds an integer temperature and
a fractional humidity. Humidity is normally displayed as a percentage, and "58%"
is the same as "0.58" which is easier to use in programs.

If we wanted to use this in a program, it would look like this.

```c
#include <stdio.h>

struct Weather {
    int temp;
    float humidity;
}
```

in
