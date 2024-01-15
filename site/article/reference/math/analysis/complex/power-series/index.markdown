---
title: Power series
description: Personal notes on power series in complex analysis.
published: 2024-01-15
include-math: true
include-bibliography-stylesheet: true
toc: true
---

Personal notes on power series in complex analysis.

Notes taken while taking a course [@coursera:complex-analysis].

## Power series {#power-series}

A _power series_ centered at \(z_0 \in \Complex\) is a series of the form

\[ \sum_{k=0}^\infty a_k (z-z_0)^k \]

Power series are also known as Taylor series.

## Convergence {#convergence}

### Absolute convergence {#absolute-convergence}

A series \(\sum_{k=0}^\infty\) _converges absolutely_
if the series \(\sum_{k=0}^\infty |a_k|\) converges.

If \(\sum_{k=0}^\infty a_k\) converges absolutely, then it also converges,
and \(\left|\sum_{k=0}^\infty a_k\right| \leq \sum_{k=0}^\infty |a_k|\).

### Radius of convergence {#convergence-radius}

For a power series \(\sum_{k=0}^\infty a_k (z-z_0)^k\),
there exists a number $0 \leq R \leq \infty$ such that
the series converges absolutely in \(\{|z-z_0| < R\}\)
and diverges in \(\{|z-z_0| > R\}\).
The convergence is uniform in \(|z-z_0| \leq r\) for any \(0 \leq r < R\).

## Analyticity of power series {#analytic}

If \(f(z)=\sum_{k=0}^\infty a_k(z-z_0)^k\) has radius of convergence \(R>0\),
$f(z)$ is analytic in \(\{|z-z_0|<R\}\).

The series can be differentiated term by term.

\begin{align*}
\frac{df}{dz}(z) & = \sum_{k=1}^\infty a_k k(z-z_0)^{k-1} \\
\frac{d^2 f}{dz^2}(z) & = \sum_{k=2}^\infty a_k k(k-1)(z-z_0)^{k-2} \\
& \vdots
\end{align*}

The coefficient $a_k$ can be derived using the $k$-order derivative.

\[ a_k = \frac{1}{k!} \frac{d^k f}{dz^k}(z_0) \]

## Integration of power series {#integraion}

If \(\sum_{k=0}^\infty a_k(z-z_0)^k\) has radius of convergence $R$,
then for any $w$ such that \(|w-z_0| < R\),

\begin{align*}
\int_{z_0}^w \sum_{k=0}^\infty a_k(z-z_0)^k \, dz
&= \sum_{k=0}^\infty a_k \int_{z_0}^w (z-z_0)^k \, dz \\
&= \sum_{k=0}^\infty \frac{a_k}{k+1}(w-z_0)^{k+1}
\end{align*}

## References
