---
title: Distributions in Statistics
published: 2023-02-15
description: Basic reference on distributions in statistics.
include-math: true
---

Basic reference on distributions used in statistics.

## Discrete distributions

### Bernoulli distribution

Single trial whose outcome can be success or failure.

\[ P(X = 1) = p \]
\[ P(X = 0) = 1-p \]

### Geometric distribution

Number of trials until first success.

For \(n = 1, 2, \ldots\),

\[ P(X = n) = (1-p)^{n-1}p \]

### Binomial distribution

Number of successes in \(n\) trials.

For \(0 \leq k \leq n\),

\[ P(X = k) = {n \choose k} = \frac{n!}{k! (n-k)!} \]

### Poisson distribution

Distribution of rare events in a large population.

For \(n = 0, 1, 2, \ldots\),

\[ P(X = n) = \frac{e^{-\lambda} \lambda^n}{n!} \]
