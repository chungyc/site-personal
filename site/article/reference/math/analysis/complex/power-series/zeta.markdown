---
title: Riemann zeta function
description: Personal notes on the Riemann zeta function and the Riemann hypothesis.
published: 2024-01-15
include-math: true
include-bibliography-stylesheet: true
toc: true
---

Personal notes on the Riemann zeta function and the Riemann hypothesis.

Notes taken while taking a course [@coursera:complex-analysis].

## Riemann zeta function {#zeta-function}

For \(s \in \Complex\) with \(\mathrm{Re}(s) > 1\),

\[ \zeta(s) = \sum_{n=1}^\infty \frac{1}{n^s} \]

\(\sum_{n=1}^\infty \frac{1}{n^s}\) converges for all \(\mathrm{Re}(s) > 1\).
The zeta function has an analytic continuation into \(\Complex - \{1\}\).

All $s$ such that \(\zeta(s)=0\) satisifies
either $s=-2n$ for $n>0$ or \(0 < \mathrm{Re}(s) < 1\).

## Riemann hypothesis {#riemann-hypothesis}

All $s$ such that \(\zeta(s)=0\) and \(0 < \mathrm{Re}(s) < 1\)
satisfies \(\mathrm{Re}(s) = \frac{1}{2}\).

## Relation to prime numbers {#primes}

### Real zeta function {#real}

For \(s \in \mathbb{R}\), thanks to unique prime factorization, it is the case that

\[ \sum_{n=1}^\infty \frac{1}{n^s}
 = \prod_p \sum_{k=0}^\infty \frac{1}{p^{ks}}
 = \prod_p \frac{1}{1-p^{-s}}
\]

### Prime number theorem {#prime-number-theorem}

$\pi(x)$, the _prime counting function_ whose value is
the number of prime numbers less than or equal to $x$,
is asymptotic to \(\frac{x}{\ln x}\) as \(x \rightarrow \infty\).

\[ \pi(x) \sim \frac{x}{\ln x} \]

This is equivalent to

\[ \lim_{x \rightarrow \infty} \frac{\pi(x)}{\frac{x}{\ln x}} = 1 \]

### Tight error bound {#equivalence}

The Riemann hypothesis is equivalent to

\[ \left| \pi(x) - \mathrm{li}(x) \right| < \frac{\sqrt{x} \ln x}{8\pi} \]

where

\[ \mathrm{li}(x) = \int_0^x \frac{1}{\ln t} \, dt \]

## See also

*   [Power series](./)
*   [Complex analysis](../)

## References
