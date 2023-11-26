---
title: Complex analysis
description: Basic reference on complex analysis.
published: 2023-10-20
include-math: true
include-bibliography-stylesheet: true
toc: true
---

Basic reference on complex analysis.

Notes taken while taking a course [@coursera:complex-analysis].

## Basics {#basics}

Disk
:   \( B_r(z_0) = \{ z \in \Complex \mid |z-z_0| < r \} \)

Circle
:   \( K_r(z_0) = \{ z \in \Complex \mid |z-z_0| = r \} \)

Interior point
:   For \( E \subset \Complex \), $z_0$ is an _interior point_ of $E$
    if there is \( r > 0 \) such that \( B_r(z_0) \subset E \).

Boundary point
:   For \( E \subset \Complex \), $b$ is a _boundary point_ of $E$
    if for all $r>0$, $E \cap B_r(b) \neq \emptyset$
    and $E^\complement \cap B_r(b) \neq \emptyset$.

    The set of all boundary points of $E$ is the _boundary set_ of $E$,
    denoted $\partial E$.

Open set
:   A set in $\Complex$ is _open_ if all of its points are interior points.

Closed set
:   A set $E$ in $\Complex$ is _closed_ if $\partial E \subset E$.

Closure
:   The _closure_ of $E$ is $\overline{E} = E \cup \partial E$.

Interior
:   The _interior_ of $E$ is the set $\overset{\circ}{E}$ of all interior points.

## Limit {#limit}

If for all \(\varepsilon > 0\) there is \(\delta > 0\) such that
\(|f(z) - c| < \varepsilon\) whenever \(|z-z_0| < \delta\), then

\[ \lim_{z \rightarrow z_0} f(z) = c \]

$f$ is _continuous_ at $z_0$ if

\[ \lim_{z \rightarrow z_0} f(z) = f(z_0) \]

## Derivative {#derivative}

\[ \frac{d}{dz} f(z_0) = \lim_{z \rightarrow z_0} f(z) \]

$f$ is _analytic_ in an open set $U \subset \Complex$ if $f$ is differentiable for every $z \in U$.
A function which is analytic in $\Complex$ is an _entire_ function.

## Cauchy-Riemann equations {#cauchy-riemann}

If $f(z) = u(x,y) + i v(x,y)$ for $z = x+iy$ and real functions $u$ and $v$,

\[ \frac{\partial u}{\partial x} = \frac{\partial v}{\partial y} \]
\[ \frac{\partial u}{\partial y} = -\frac{\partial v}{\partial x} \]

Also,

\[ \frac{d f}{d z} = \frac{\partial f}{\partial x} = -i\frac{\partial f}{\partial y} \]

$f=u+iv$ is analytic in $D$ if and only if for any $z \in D$,
$\frac{\partial u}{\partial x}$ and $\frac{\partial v}{\partial y}$ exist,
are continuous, and satisfy the Cauchy-Riemann equations.

## Fundamental theorem of algebra {#algebra-theorem}

If $a_0$, $\ldots$, $a_n$ are complex numbers with $a_n \neq 0$, then the polynomial

\[ p(z) = \sum_{k=0}^n a_k z^k \]

has $n$ complex roots $z_1$, $\ldots$, $z_n$, where

\[ p(z) = a_n \prod_{k=1}^n (z - z_k) \]

## References
