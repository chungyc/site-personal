---
title: Passage of time in special relativity
description: |
  How to think about the passage of time for an object following a world line.
  Explains the concept of proper time.
published: 2023-05-05
toc: true
include-math: true
include-bibliography-stylesheet: true
---

Discusses how time passes for a non-inertial observer in special relativity [@carroll2022; @susskind2017].

## World lines

For any object moving through space, one can plot its spatial position
against its temporal position with a [spacetime diagram].
For an object which stays in a single inertial frame,
the world line of this object will be a straight line.

World lines do not have to be straight lines.
We could also have objects which accelerate randomly and do not stay in
a single inertial frame, and we would have no problem plotting a world line
for the object as in the diagram below.

![A wandering world line](/diagrams/article/relativity/proper-time/random-line.svg)

For objects which stay in a single inertial frame, we can use the equations
for [time dilation] to figure out how time passes for these objects
relative to another inertial frame.  But what about objects which do not
stay in a single inertial frame?  What do they experience relative to
a particular inertial frame, and how would we figure out how time passes
for these objects?

[spacetime diagram]: /article/physics/relativity/spacetime-diagrams
[time dilation]: /article/physics/relativity/spacetime-diagrams#time-dilation

## Spacetime intervals

Before thinking about how time passes for moving objects in general,
let's think about how it passes for an object moving at a constant velocity.
In other words, an object that stays in a single inertial frame.
The spacetime diagram on the left below shows the world line of such an object.
To get the amount of time that passes for this object,
we measure it in the inertial frame where the moving object is stationary,
which is the spacetime diagram on the right below.

![Measuring time for a moving object](/diagrams/article/relativity/proper-time/measuring-time.svg)

We can also find the time which passes for the moving object without transforming
a world line into the intertial frame where it is stationary.
First, we can define the _spacetime interval_ as follows.

\[ ds^2 = dt^2 - dx^2 - dy^2 - dz^z \]

It can be shown that this remains the same no matter the inertial frame.
In other words, the amount of time or length between two events will be
different depending on the inertial frame, but the spacetime interval
between two events will stay the same.

In the inertial frame where a world line is not moving, we have

\[ dx = dy = dz = 0 \]

so it must be the case that

\[ ds^2 = dt^2 \]

In other words, the time which passes for an object moving at constant velocity
is the same as its spacetime interval.  To distinguish the time which passes for
the object from the time as observed from other inertial frames,
we often call the former the _proper time_ for the object,
and \(\tau\) is the symbol which usually denotes the proper time.
So we can say that the proper time for an inertial world line
is the same as its spacetime interval.

\[ d\tau = ds \]

## Proper time in general

Now that we know how to figure out the time which passes for an object
moving at constant velocity, how would we figure it out for an object
which does not stay in a single intertial frame?

If a moving object switches between a finite number of inertial frames as
in the spacetime diagram below, then it is a simple matter of adding up
the time which passes for the object in each of these inertial frames.

![Switching between inertial frames](/diagrams/article/relativity/proper-time/discrete-lines.svg)

We know that the time which passes for the object in each segment is equal
to the spacetime interval of the segment, so the total proper time
is the sum of these spacetime intervals.

\[ \tau = \sum \Delta\tau = \sum \Delta s \]

What about an object which changes its velocity smoothly as in the spacetime diagram below?

![Continuously transitioning between inertial frames](/diagrams/article/relativity/proper-time/continuous-line.svg)

We can think of this world line as an infinite number of infinitely small segments
of straight world lines, each of them continuously switching over to different inertial frames.
The total proper time would be the sum of these,
and anyone who has even a passing familiarity with calculus would recognize
that this is the same as the integral over the spacetime interval.

\[ \tau = \int d\tau = \int ds \]

Given that \( ds = \sqrt{dt^2 - dx^2 - dy^2 - dz^2} \),
this gives us a way to figure out the proper time for any world line
without having to transform infinitely tiny bits of spacetime diagrams
an infinite number of times.

### With parameterized coordinates

If you are computing the proper time numerically by chopping up a world line into
little pieces and adding up the spacetime intervals, this is enough.
It's a lot harder if you want to derive an analytical solution in the form of an equation.
I don't even know where to start with integrals of the form
\( \int \sqrt{dt^2 -dx^2 - dy^2 - dz^2} \).

It is easier if it is changed into an integral of a single variable.
This can be done by _parameterizing_ the coordinates \(t\), \(x\), \(y\), \(z\) with
a single variable, let's say, \(\theta\).  In other words, they would be functions
of the form \(t(\theta)\), \(x(\theta)\), \(y(\theta)\), \(z(\theta)\),
taking advantage of the fact that a world line is a line and not something like a surface.[^surface]
The integral for proper time would then be of a more tractable sort.

\[
\tau = \int \sqrt{\left(\frac{dt}{d\theta}\right)^2 -
                  \left(\frac{dx}{d\theta}\right)^2 -
                  \left(\frac{dx}{d\theta}\right)^2 -
                  \left(\frac{dx}{d\theta}\right)^2} \, d\theta
\]

While it can be the case that \(\theta = \tau\), it doesn't have to be.[^proper-time-parameter]
The variable \(\theta\) may not have any special physical meaning,
since the most important factor in deciding how to parameterize the coordinates
would be to make the calculations simpler, not that \(\theta\) have any
physical significance.

With all that said, I expect most such integrals would be beyond my ability to solve analytically.
I'm satisfied with understanding the concept that proper time
is equal to the sum of the spacetime intervals and being able
to have a computer calculate the sum numerically to get the proper time.

[^surface]: A world "surface" can be parameterized, but it would need two variables.

[^proper-time-parameter]: In fact, if we are trying to compute the proper time,
  it will almost certainly be the case that \(\theta \neq \tau\),
  since if they were equal, we would already know \(\tau\)
  and wouldn't be trying to do complicated calculations to obtain the proper time.

## Staying still for the longest time

## See also

*   [Spacetime diagrams](/article/physics/relativity/spacetime-diagrams)

## References
