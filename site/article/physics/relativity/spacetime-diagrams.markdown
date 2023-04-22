---
title: Spacetime diagrams
description: Spacetime diagrams for plotting out wordlines relativistically.
published: 2023-04-22
toc: true
include-math: true
---

## Spacetime

Special relativity starts out with the following two postulates:

1.   The laws of physics are the same in all inertial frames.
2.   The speed of light is the same in all inertial frames.[^maxwell]

[^maxwell]: With [Maxwell's equations for electromagnetism],
one could argue that the second postulate is a consequence of the first postulate.
This is because the equations predict a specific speed for light,
and the equations would not be laws of physics
if the speed of light was different for different inertial frames.

With these postulates, one can derive how the coordinates \((x,t)\) of a particular event
in one inertial frame are transformed to coordinates \((x',t')\) in another inertial frame as follows,

\begin{aligned}
t' & = \gamma \left( t - \frac{vx}{c^2} \right) \\
x' & = \gamma (x - vt)
\end{aligned}

where \(v\) is the speed the other inertial frame is moving relative to the original inertial frame,
and the Lorentz factor \(\gamma\) is

\[ \gamma = \frac{1}{\sqrt{1 - \left( \frac{v}{c} \right)^2}} \]

In other words, the coordinates for space and time do not transform independently.
Instead, they transform together as a set of spacetime coordinates from one inertial frame to another.
Space and time should not be considered completely independent things,
but should be considered intertwined as spacetime.

## A spacetime diagram

![A spacetime diagram](/diagrams/article/relativity/diagrams/worldline.svg)

## From another inertial frame

![Same diagram from observer moving at \(0.5c\)](/diagrams/article/relativity/diagrams/worldline-50.svg)

## Time dilation

\[ t' = \gamma t \]

![Time dilation at \(0.9c\)](/diagrams/article/relativity/diagrams/time-dilation.svg)

## Length contraction

\[ l' = \frac{1}{\gamma} l \]

![Length contraction at \(0.9c\)](/diagrams/article/relativity/diagrams/length-contraction.svg)

## Simultaneity

![Simultaneity is relative](/diagrams/article/relativity/diagrams/simultaneity.svg)

## Conclusion

[Maxwell's equations for electromagnetism]: https://www.maxwells-equations.com/
