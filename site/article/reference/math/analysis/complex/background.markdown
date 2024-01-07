---
title: Background definitions for complex analysis
description: Background definitions for use in complex analysis.
published: 2024-01-07
include-math: true
include-bibliography-stylesheet: true
toc: true
---

Background definitions used in my personal notes for [complex analysis](./).

Notes taken while taking a course [@coursera:complex-analysis].

## Disk {#disk}

\( B_r(z_0) = \{ z \in \Complex \mid |z-z_0| < r \} \)

### Circle {#circle}

\( K_r(z_0) = \{ z \in \Complex \mid |z-z_0| = r \} \)

## Points {#points}

### Interior point {#interior}

For \( E \subset \Complex \), $z_0$ is an _interior point_ of $E$
if there is \( r > 0 \) such that \( B_r(z_0) \subset E \).

### Boundary point {#boundary}

For \( E \subset \Complex \), $b$ is a _boundary point_ of $E$
if for all $r>0$, $E \cap B_r(b) \neq \emptyset$
and $E^\complement \cap B_r(b) \neq \emptyset$.

The set of all boundary points of $E$ is the _boundary set_ of $E$,
denoted $\partial E$.

## Sets {#sets}

### Open set {#open}

A set in $\Complex$ is _open_ if all of its points are interior points.

### Closed set {#closed}

A set $E$ in $\Complex$ is _closed_ if $\partial E \subset E$.

### Closure {#closure}

The _closure_ of $E$ is $\overline{E} = E \cup \partial E$.

### Interior {#interior}

The _interior_ of $E$ is the set $\overset{\circ}{E}$ of all interior points.

## Path {#path}

A _path_ in the complex plane is a continuous function
from a real number interval $[a,b]$ to complex numbers.

### Curve {#curve}

A _curve_ is a smooth path or a piecewise smooth path.

### Smooth path {#smooth}

A path is _smooth_ if the function can be differentiated an arbitrary number of times.

### Piecewise smooth path {#piecewise-smooth}

A path is _piecewise smooth_ if it is the concatenation of a finite number of smooth paths.

## References
