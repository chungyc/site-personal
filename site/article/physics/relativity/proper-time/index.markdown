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

![Measuring time for a moving object](/diagrams/article/relativity/proper-time/measuring-time.svg)

One can define the _spacetime interval_ as follows.

\[ ds^2 = dt^2 - dx^2 - dy^2 - dz^z \]

It can be shown that this remains the same no matter the inertial frame.
In other words, the amount of time or length between two events will be
different depending on the inertial frame, but the spacetime interval
between two events will stay the same.

\[ dx = dy = dz = 0 \]

\[ ds^2 = dt^2 \]

## Proper time

\[ d\tau = ds \]

![Switching between inertial frames](/diagrams/article/relativity/proper-time/discrete-lines.svg)

\[ \tau = \sum \Delta\tau = \sum \Delta s \]

![Continuously transitioning between inertial frames](/diagrams/article/relativity/proper-time/continuous-line.svg)

\[ \tau = \int d\tau = \int ds \]

\[ ds = \sqrt{dt^2 - dx^2 - dy^2 - dz^2} \]

## Staying still for the longest time

## See also

*   [Spacetime diagrams](/article/physics/relativity/spacetime-diagrams)

## References
