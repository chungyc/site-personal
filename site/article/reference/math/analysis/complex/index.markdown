---
title: Complex analysis
description: Basic reference on complex analysis.
published: 2023-10-20
updated: 2024-01-11
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

### Cauchy-Riemann equations {#cauchy-riemann}

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

\[ \int_\gamma f(z) \, dz = \int_a^b f(\gamma(t)) \, \gamma'(t) \, dt \]

### By arc length {#integral-arc-length}

Integration with respect to arc length is defined as

\[ \int_\gamma f(z) \, |dz| = \int_a^b f(\gamma(t)) \, |\gamma'(t)| \, dt \]

#### ML Estimate {#ml-estimate}

If $\gamma$ is a curve and $f$ is continuous on $\gamma$,

\[ \left| \int_\gamma f(z) \, dz \right| \leq \int_\gamma |f(z)| \, |dz| \]

In particular, if $|f(z)| \leq M$ on $\gamma$,

\[ \left| \int_\gamma f(z) \, dz \right| \leq M \cdot \mathrm{length}(\gamma) \]

### Primitives {#primitive}

If $D \subset \Complex$ and $f : D \rightarrow \Complex$ is continuous,
a _primitive_ of $f$ is an analytic function $F : D \rightarrow \Complex$
such that \(\frac{dF}{dz} = f\) on $D$.
For any curve $\gamma : [a,b] \rightarrow D$,

\[ \int_\gamma f(z) \, dz = F(\gamma(b)) - F(\gamma(a)) \]

If $D$ is a **simply** connected domain in $\Complex$ and $f$ is analytic in $D$,
then $f$ has a primitive in $D$.

### Cauchy's theorem {#cauchy-theorem}

If $D$ is a simply connected domain in $\Complex$, $f$ is analytic in $D$,
and $\gamma$ is a piecewise smooth and closed curve in $D$, then

\[ \int_\gamma f(z) \, dz = 0 \]

If $\gamma_1$ and $\gamma_2$ are simply closed curves with the same orientation,
$\gamma_2$ is inside $\gamma_1$, and $f$ is analytic in a domain which contains both curves
and the region between them, then

\[ \int_{\gamma_1} f(z) \, dz = \int_{\gamma_2} f(z) \, dz \]

### Cauchy integral formula {#cauchy-integral-formula}

If $D$ is a simply connected domain bounded by a piecewise smooth curve $\gamma$,
and $f$ is analytic in a superset of $\overline{D}$, then for all $w \in D$

\[ f(w) = \frac{1}{2 \pi i} \int_\gamma \frac{f(z)}{z-w} \, dz \]

If $f$ is analytic in an open set,
then $\frac{df}{dz}$ is also analytic in the same open set.
For all $w \in D$ and $k \geq 0$,

\[ \frac{d^k f}{dz^k}(w)
 = \frac{k!}{2 \pi i} \int_\gamma \frac{f(z)}{(z-w)^{k+1}} \, dz
\]

#### Cauchy's estimate {#cauchy-estimate}

If $f$ is analytic in an open set which contains $\overline{B_r(z_0)}$
and $|f(z)| \leq m$ holds on $\partial B_r(z_0)$, then for all $k \geq 0$,

\[ \left| \frac{d^k f}{dz}(z_0) \right| \leq \frac{k! \, m}{r^k} \]

## Fundamental theorem of algebra {#algebra-theorem}

If $a_0$, $\ldots$, $a_n$ are complex numbers with $a_n \neq 0$, then the polynomial

\[ p(z) = \sum_{k=0}^n a_k z^k \]

has $n$ complex roots $z_1$, $\ldots$, $z_n$, where

\[ p(z) = a_n \prod_{k=1}^n (z - z_k) \]

## See also

*   [Some complex functions](functions/)
*   [Conformal mappings](conformal-mappings/)

## References
