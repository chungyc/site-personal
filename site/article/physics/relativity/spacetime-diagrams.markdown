---
title: Spacetime diagrams
description: Spacetime diagrams for plotting out world lines relativistically.
published: 2023-04-24
toc: true
include-math: true
include-bibliography-stylesheet: true
---

Plotting world lines on spacetime diagrams can aid in the understanding of
special relativity through visualization.

## Spacetime {#spacetime}

Special relativity [@carroll2022; @susskind2017] starts out with
the following two postulates:

1.   The laws of physics are the same in all inertial frames.
2.   The speed of light is the same in all inertial frames.[^maxwell]

[^maxwell]: With [Maxwell's equations for electromagnetism],
one could argue that the second postulate is a consequence of the first postulate.
This is because the equations predict a specific speed for light,
and the equations would not be laws of physics
if the speed of light was different for different inertial frames.

With these postulates, one can derive how the coordinates \((x,t)\)
of a particular event in one inertial frame are transformed
to coordinates \((x',t')\) in another inertial frame as follows,[^ignored-yz]

\begin{aligned}
t' & = \gamma \left( t - \frac{vx}{c^2} \right) \\
x' & = \gamma (x - vt)
\end{aligned}

where \(v\) is the speed the other inertial frame is moving relative to the original inertial frame,
and the Lorentz factor \(\gamma\) is

\[ \gamma = \frac{1}{\sqrt{1 - \left( \frac{v}{c} \right)^2}} \]

[^ignored-yz]: Space in our world is three-dimensional, so there should also
be \(y\) and \(z\) coordinates, but we ignore them here, both because they
remain unchanged when the movement is in the \(x\) direction,
and because it makes drawing spacetime diagrams on a two-dimensional
surface much simpler.

In other words, the coordinates for space and time do not transform independently.
Instead, they transform together as a set of spacetime coordinates from one inertial frame to another.
Space and time should not be considered completely independent things,
but should be considered intertwined as spacetime.

## A spacetime diagram {#diagram}

If you have a record of how a particular object moves across space as time passes by,
you have a record of \((x,t)\) spacetime coordinatates for the object.
You can plot these on a graph with an \(x\) axis and a \(t\) axis.
This is called a _spacetime diagram_, and the plot of the \((x,t)\) coordinates
is called a _world line_.  These are useful for visualizing things in spacetime.

Below is a spacetime diagram with a world line of an object departing from the origin at
half the speed of light, or \(v=0.5c\).  The horizontal axis is the \(x\) axis,
and the vertical axis is the \(t\) axis with the future pointing up.
The two dashed line criss-crossing at the origin
are the tracks of light passing through the origin, and these are often called
the _light cone_ in a spacetime diagram.

![A spacetime diagram with a \(0.5c\) world line](/diagrams/article/relativity/diagrams/worldline.svg)

There is no reason why one cannot use spacetime diagrams with non-relativistic mechanics,
but the different way coordinates transform between different inertial frames
makes spacetime diagrams even more useful for visualizing things in relativity.

## From another inertial frame {#transform}

In non-relativistic mechanics, transforming coordinates from one inertial frame
to another does not change the \(t\) coordinate or spatial lengths.
If one were to transform the spacetime diagram in the previous section to the inertial
frame for the moving object, it would merely slant the diagram.
Horizontal and vertical distances would not change.
In particular, the world line corresponding to the \(x\) axis
in the original diagram would remain the same in the other inertial frame.
In addition, light emitted from a stationary source in the original frame
would have a different speed.

![Same diagram from observer moving at \(0.5c\) in a non-relativistic world](/diagrams/article/relativity/diagrams/worldline-nonrelativistic.svg)

However, this is not how coordinates actually transform in the real world given its relativistic nature.
Unlike in non-relativistic mechanics, where space does not affect how time is transformed
and there is no distortion in the spatial dimensions with relative velocities,
space does affect how time is transformed and there is distortion according to the Lorentz factor
for both temporal and spatial dimensions.

When the spacetime diagram from the [previous section](#diagram) is tranformed into
the inertial frame for the object moving at half the speed of light, it will transform
as in the following diagram.

![Same diagram from observer moving at \(0.5c\) in the real world](/diagrams/article/relativity/diagrams/worldline-50.svg)

As in the diagram transformed non-relativistically, the world line of the observer is stationary
in its own inertial frame, i.e., \(x=0\) for the world line.  But much else is different.
Both the \(x\) and \(t\) axes are different from those in the original inertial frame,
and events that had the same \(t\) coordinate in the original inertial frame now have
different \(t\) coordinates.  And the speed of light remains the same in both inertial frames.

You may or may not have also noticed that the world line now terminates at a \(t\) coordinate
slightly smaller than the \(t\) coordinate it terminated at in the original diagram.
If you did, you noticed time dilation, which will be discussed in the [next section](#time-dilation).

## Time dilation {#time-dilation}

The passage of time \(t\) in one inertial frame is \( t' = \gamma t \) in another inertial frame.
This time dilation can be visualized using spacetime diagrams.

In the left diagram below is a world line stationary at \(x=0\),
starting at \(t=0\) up to some time interval.
On its right is the same diagram in another inertial frame moving at \(0.9c\)
towards the left, relative to the original inertial frame.
You can see that the world line on the right terminates at a \(t\) coordinate
significantly higher than where it terminates on the left.

![Time dilation at \(0.9c\)](/diagrams/article/relativity/diagrams/time-dilation.svg)

What does this mean?  It means that time which elapses for the world line
in the inertial frame on the right is significantly longer than how much it
elapses in the inertial frame on the left.  In other words, the time for
a stationary observer is dilated for an observer moving at \(0.9c\).
The stationary observer does not notice anything funny with the flow
of time, but the moving observer will notice that time is flowing much
more slowly for the stationary observer.

## Length contraction {#length-contraction}

\[ l' = \frac{1}{\gamma} l \]

![Length contraction at \(0.9c\)](/diagrams/article/relativity/diagrams/length-contraction.svg)

## Simultaneity {#simultaneity}

![Simultaneity is relative](/diagrams/article/relativity/diagrams/simultaneity.svg)

## Conclusion {#conclusion}

## References



<!-- List of reference links -->

[Maxwell's equations for electromagnetism]: https://www.maxwells-equations.com/
