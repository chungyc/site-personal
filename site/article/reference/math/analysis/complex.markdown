---
title: Complex analysis
description: Basic reference on complex analysis.
published: 2023-10-20
include-math: true
toc: true
---

Basic reference on complex analysis.

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

## Fundamental theorem of algebra {#algebra-theorem}

If $a_0$, $\ldots$, $a_n$ are complex numbers with $a_n \neq 0$, then the polynomial

\[ p(z) = \sum_{k=0}^n a_k z^k \]

has $n$ complex roots $z_1$, $\ldots$, $z_n$, where

\[ p(z) = a_n \prod_{k=1}^n (z - z_k) \]
