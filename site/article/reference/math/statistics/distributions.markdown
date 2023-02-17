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

## Continuous distributions

### Uniform distribution

For \(a \leq x \leq b\),

\[ f(x) = \frac{1}{b-a} \]

### Exponential distribution

For \(x \geq 0\),

\[ f(x) = \lambda e^{-\lambda x} \]

### Normal distribution

\[ f(x) = \frac{1}{\sqrt{2 \pi \sigma^2}} e^{-\frac{1}{2 \sigma^2} (x - \mu)^2} \]

### Gamma distribution

For \(x \geq 0\),

\[ f(x) = \frac{1}{\Gamma(\alpha)} \beta^\alpha x^{\alpha-1} e^{-\beta x} \]
\[ \Gamma(\alpha) = \int_0^\infty x^{\alpha-1} e^{-x} \, dx \]

#### Properties of the Gamma function

For \( \alpha = 1 \),

\[ \Gamma(1) = 1 \]

For \( \alpha > 1 \),

\[ \Gamma(\alpha) = (\alpha - 1) \Gamma(\alpha - 1) \]

For integer \(n \geq 1\),

\[ \Gamma(n) = (n-1)! \]
