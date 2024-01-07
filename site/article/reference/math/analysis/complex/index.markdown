---
title: Complex analysis
description: Basic reference on complex analysis.
published: 2023-10-20
updated: 2024-01-07
include-math: true
include-bibliography-stylesheet: true
toc: true
---

Basic reference on complex analysis.
See [background definitions](background) for prerequisites.

Notes taken while taking a course [@coursera:complex-analysis].

## Limit {#limit}

If for all \(\varepsilon > 0\) there is \(\delta > 0\) such that
\(|f(z) - c| < \varepsilon\) whenever \(|z-z_0| < \delta\), then

\[ \lim_{z \rightarrow z_0} f(z) = c \]

$f$ is _continuous_ at $z_0$ if

\[ \lim_{z \rightarrow z_0} f(z) = f(z_0) \]

## Derivative {#derivative}

\[ \frac{d}{dz} f(z_0) = \lim_{z \rightarrow z_0} \frac{f(z) - f(z_0)}{z-z_0} \]

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

## Integral

The path integral over a path $\gamma: [a,b] \rightarrow \Complex$ is

\[ \int_\gamma f(z) \, dz = \lim_{n \rightarrow \infty} \sum_{k=0}^{n-1} f(z_k) (z_{k+1} - z_k) \]

where \(z_k = \gamma(t_k)\) and \(a = t_0 < t_1 < \ldots < t_n = b\) for any \(n>0\).

If $\gamma$ is a smooth curve and $f$ is continuous,

\[ \int_\gamma f(z) \, dz = \int_a^b f(\gamma(t)) \gamma'(t) \, dt \]

## Fundamental theorem of algebra {#algebra-theorem}

If $a_0$, $\ldots$, $a_n$ are complex numbers with $a_n \neq 0$, then the polynomial

\[ p(z) = \sum_{k=0}^n a_k z^k \]

has $n$ complex roots $z_1$, $\ldots$, $z_n$, where

\[ p(z) = a_n \prod_{k=1}^n (z - z_k) \]

## See also

*   [Some complex functions](functions/)
*   [Conformal mappings](conformal-mappings/)

## References
