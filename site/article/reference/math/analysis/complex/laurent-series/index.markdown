---
title: Laurent series
description: Personal notes on Laurent series in complex analysis.
published: 2024-01-22
updated: 2024-01-28
include-math: true
include-bibliography-stylesheet: true
toc: true
---

Personal notes on Laurent series in complex analysis.

Notes taken while taking a course [@coursera:complex-analysis].

## Laurent series {#series}

If \(f:U \rightarrow \Complex\) is analytic and \(D = \{r < |z-z_0| < R\} \subset U\),
then there is a sequence $a_k$ where $k \in \mathbb{Z}$ such that

\[ f(z) = \sum_{k=-\infty}^\infty a_k (z-z_0)^k \]

This is a _Laurent series expansion_ for $f$.  It converges in $D$,
and it converges absolutely and uniformly for any \(\{s < |z-z_0| < t\}\)
where \(r < s < t < R\).

## Coefficients {#coefficients}

If $f$ is analytic in \(\{ r < | z - z_0 | < R \}\), and $a_k$ is such that

\[ a_k = \frac{1}{2 \pi i} \int_{|z-z_0|=s} \frac{f(z)}{(z-z_0)^{k+1}} \, dz \]

for any $s$ where \(r < s < R\), then

\[ f(z) = \sum_{k=-\infty}^\infty a_k (z-z_0)^k \]

## Singularities {#singularities}

A singularity $z_0$ is an _isolated singularity_ of $f$
if $f$ is analytic in \(\{ 0 < | z - z_0 | < r \}\) for some $r>0$.

If $z_0$ is an isolated singularity for \( f(z) = \sum_{k=-\infty}^\infty a_k (z-z_0)^k \),
then the singularity $z_0$ is

*   _removable_ if \(a_k=0\) for all \(k<0\)
*   a _pole_ if there is some $N$ such that \(a_N \neq 0\) and \(a_k = 0\) for all \(k < -N\);
    $N$ is the _order_ of the pole
*   _essential_ if \(a_k \neq 0\) for infinitely many \(k<0\)

### Removable {#removable-singularities}

An isolated singularity $z_0$ of $f$ is removable if and only if $f$ is bounded near $z_0$.

### Pole {#poles}

An isolated singularity $z_0$ of $f$ is a pole
if and only if \(\lim_{z \rightarrow z_0} |f(z)| = \infty\).

### Essential {#essential-singularities}

If $z_0$ is an essential singularity of $f$, then for every \(w \in \Complex\)
there exists a sequence \(\{z_n\}\) such that

\[ \lim_{n \rightarrow \infty} z_n = z_0 \]

\[ \lim_{n \rightarrow \infty} f(z_n) = w \]

In addition, for every \(w \in \Complex\) with at most one exception,
there exists a sequence \(\{z_n\}\) such that

\[ \lim_{n \rightarrow \infty} z_n = z_0 \]

\[ f(z_n) = w \]

## Residue theorem {#residue-theorem}

If these are the case:

*   $D$ is a simply connected domain
*   $f$ is analytic in $D$ except for isolated singularities
*   $C$ is a simple closed curve oriented counterclockwise
*   $z_1$, ..., $z_n$ are the isolated singularities of $f$ which lie inside $C$

then the following holds:

\[ \int_C f(z) \, dz = 2 \pi i \sum_{k=1}^n \mathrm{Res}(f, z_k) \]

If $z_0$ is an isolated singularity of $f$, the _residue_ of $f$ at $z_0$ is $a_{-1}$,
where \( f(z) = \sum_{k=-\infty}^\infty a_k (z - z_0)^k \).

\[ \mathrm{Res}(f, z_0) = a_{-1} \]

If an isolated singularity $z_0$ of $f$ is removable,

\[ \mathrm{Res}(f, z_0) = 0 \]

If $z_0$ is a simple pole,

\[ \mathrm{Res}(f, z_0) = \lim_{z \rightarrow z_0} (z - z_0) f(z) \]

More generally, if $z_0$ is a pole of order $n$,

\[ \mathrm{Res}(f, z_0)
 = \frac{1}{(n-1)!} \lim_{z \rightarrow z_0}
     \frac{d^{n-1}}{dz^{n-1}} \left( (z-z_0)^n f(z) \right)
\]

If \(f(z) = \frac{g(z)}{h(z)}\), where $g$ and $h$ are analytic near $z_0$
and $h$ has a simple zero at $z_0$,

\[ \mathrm{Res}(f, z_0) = \frac{g(z_0)}{\frac{dh}{dz}(z_0)} \]

## See also

*   [Power series](../power-series/)
*   [Complex analysis](../)

## References
