---
title: Conformal mappings
description: Personal notes on conformal mappings in complex analysis.
published: 2023-12-05
updated: 2024-01-07
include-math: true
include-bibliography-stylesheet: true
toc: true
---

Personal notes on conformal mappings in [complex analysis](../).

Notes taken while taking a course [@coursera:complex-analysis].

## Conformal mapping {#conformal-mapping}

A smooth complex function $f$ is _conformal_ at $z$ if for any two curves $\gamma_1$ and $\gamma_2$
that intersect at $z$ with non-zero tangents, then the tangents of $f \circ \gamma_1$ and $f \circ \gamma_2$
intersects at $f(z)$ with the same angle.

A _conformal mapping_ from $D$ to $V$ is a bijection which is conformal on all points in $D$.

If \( f : D \rightarrow \Complex \) is analytic and \( f'(z_0) \neq 0 \) for \( z_0 \in D \),
then $f$ is conformal at $z_0$.

## Möbius transformations {#mobius}

A _Möbius transformation_ is a function $f$ of the form

\[ f(z) = \frac{az + b}{cz + d} \]

where $\{a,b,c,d\} \subset \Complex$ and $ad-bc \neq 0$.
A Möbius transformation is also called a _fractional linear transformation_.

Möbius transformation are conformal mappings from $\hat{\Complex}$ to $\hat{\Complex}$,
and in fact they are the only conformal mappings from $\hat{\Complex}$ to $\hat{\Complex}$.

### Extended complex plane {#extended-plane}

Includes $\infty$ in addition to the complex numbers.  In other words,

\[ \hat{\Complex} = \Complex \cup \{ \infty \} \]

### Affine transformations {#affine}

An _affine transformation_ is a Möbius transformation $f$ of the form

\[ f(z) = az + b \]

where $a \neq 0$.

Affine transformations are conformal mappings from $\Complex$ to $\Complex$,
and are in fact the only conformal mappings from $\Complex$ to $\Complex$.

### Mapping distinct points {#mapping-points}

For distinct points $z_1$, $z_2$, $z_3$, there is a unique Möbius transformation $f$ where

\[ f(z) = \frac{z-z_1}{z-z_3} \cdot \frac{z_2 - z_3}{z_2 - z_1} \]

which maps $z_1$, $z_2$, $z_3$ to $0$, $1$, $\infty$, respectively.

For distinct points $z_1$, $z_2$, $z_3$ and distinct points $w_1$, $w_2$, $w_3$,
there is a unique Möbius transformation which
maps $z_1$, $z_2$, $z_3$ to $w_1$, $w_2$, $w_3$, respectively.

### Composition {#composition}

The composition of two Möbius transformations is also a Möbius transformation.

Any Möbius transformation can be composed from the following three types of transformations:

\begin{align*}
f(z) &= az && \text{(rotation and dilation)} \\
f(z) &= z+b && \text{(translation)} \\
f(z) &= \frac{1}{z} && \text{(inversion)}
\end{align*}

### Mapping shapes {#mapping-circles}

Möbius transformations map circles and lines to circles and lines.

## Riemann mapping theorem {#riemann-mapping-theorem}

If $D$ is a simply connected domain in the complex plane, i.e., open, connected, and has no holes,
and $D$ is a strict subset of $\Complex$, then there is a conformal mapping from $D$
onto the unit disk $\mathbb{D}=B_1(0)$.

## References
