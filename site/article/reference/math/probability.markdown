---
title: Probability
published: 2023-02-10
updated: 2023-11-18
description: Basic reference on probability.
include-math: true
toc: true
---

Basic reference for probability that I find myself looking up.

## Probability {#definition}

Probability of $X$:

\[ P(X) \]

## Independence {#independence}

When $X$ and $Y$ are independent:

\[ P(X \cap Y) = P(X) P(Y) \]

## Conditional probability {#conditional}

Conditional probability of $X$ given $C$:

\[ P(X \mid C) = \frac{P(X \cap C)}{P(C)} \]

## Bayes' theorem {#bayes-theorem}

\[ P(X \mid Y) = \frac{P(Y \mid X) P(X)}{P(Y)} \]

## Convergence {#convergence}

### Convergence in probability {#convergence-probability}

A sequence of random variables $X_1, X_2, \ldots$ converges in probabilty
to a random variable $X$ if for any $\epsilon > 0$,

\[ \lim_{n \rightarrow \infty} P(|X_n - X| > \epsilon) = 0 \]

This may be denoted as

\[ X_n \xrightarrow{P} X \]

### Convergence in distribution {#convergence-distribution}

A sequence of random variables $X_1, X_2, \ldots$, each with
cumulative probability density functions $f_1, f_2, \ldots$,
converges in distribution to a random variable $X$ with
a cumulative probability density function $f$ if
for all points $x$ where $f$ is continuous,

\[ \lim_{n \rightarrow \infty} f_n(x) = f(x) \]

This may be denoted as

\[ X_n \xrightarrow{d} X \]

## Inequalities {#inequalities}

When $g$ is a non-negative function,

\[ P(g(x) \geq c) \leq \frac{\mathrm{E}[g(X)]}{c} \]

### Markov's inequality {#markov-inequality}

\[ P(|x| \geq c) \leq \frac{\mathrm{E}[|x|]}{c} \]

### Chebyshev's inequality {#chebyshev-inequality}

If $X$ is a random variable with mean $\mu$ and variance $\sigma^2$,
and $k > 0$,

\[ P(|X - \mu| \geq k\sigma) \leq \frac{1}{k^2} \]

\[ P(|X - \mu| < k\sigma) > 1 - \frac{1}{k^2} \]

## Weak law of large numbers {#weak-law-large-numbers}

If $X_1, X_2, \ldots$ is a sequence of independent and identically random variables
with mean $\mu$ and variance $\sigma^2 < \infty$,

\[ \overline{X} \xrightarrow{P} \mu \]

## Central limit theorem {#central-limit-theorem}

If $X_1, X_2, \ldots$ is a sequence of random variables from
a distribution with mean $\mu$ and variance $\sigma^2 < \infty$,

\[ \frac{\overline{X}_n - \mu}{\sigma'} \xrightarrow{d} N(0,1) \]

where $N(0,1)$ is the [standard normal distribution] and

\[ \sigma' = \frac{\sigma}{\sqrt{n}} \]
\[ \overline{X}_n = \frac{1}{n} \sum_{i=1}^n X_i \]

[standard normal distribution]: /article/reference/math/statistics/distributions#normal

### Asymptotically normal {#asymptotically-normal}

If for a random variable $X_n$ there exists sequences $a_1, a_2, \ldots$
and $b_1, b_2, \ldots$ such that

\[ \frac{X_n - a_n}{\sqrt{b_n}} \xrightarrow{d} N(0,1) \]

then $X_n$ is _asymptotically normal_.  This may be denoted as

\[ X_n \stackrel{\mathrm{asymp}}{\sim} N(a_n,b_n) \]

According to this notation, the central limit theorem can be expressed as

\[ \overline{X}_n \stackrel{\mathrm{asymp}}{\sim} N(\mu, \frac{\sigma^2}{n}) \]
