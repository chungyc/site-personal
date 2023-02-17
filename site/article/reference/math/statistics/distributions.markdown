---
title: Distributions in Statistics
published: 2023-02-15
description: Basic reference on distributions in statistics.
include-math: true
---

\newcommand{\E}[1]{\mathrm{E}[#1]}
\newcommand{\Var}[1]{\mathrm{Var}[#1]}

Basic reference on distributions used in statistics.

## Discrete distributions

### Bernoulli distribution

Single trial whose outcome can be success or failure.

\[ P(X = 1) = p \]
\[ P(X = 0) = 1-p \]
\[ \E{X} = p \]
\[ \Var{X} = p(1-p) \]

### Geometric distribution

Number of trials until first success.

For \(n = 1, 2, \ldots\),

\[ P(X = n) = (1-p)^{n-1}p \]
\[ \E{X} = \frac{1-p}{p} \]
\[ \Var{X} = \frac{1-p}{p^2} \]

### Binomial distribution

Number of successes in \(n\) trials.

For \(0 \leq k \leq n\),

\[ P(X = k) = {n \choose k} p^x (1-p)^{n-x} \]
\[ \E{X} = np \]
\[ \Var{X} = np(1-p) \]

### Poisson distribution

Distribution of rare events in a large population.

For \(n = 0, 1, 2, \ldots\),

\[ P(X = n) = \frac{e^{-\lambda} \lambda^n}{n!} \]
\[ \E{X} = \lambda \]
\[ \Var{X} = \lambda \]

## Continuous distributions

### Uniform distribution

For \(a \leq x \leq b\),

\[ f(x) = \frac{1}{b-a} \]
\[ \E{X} = \frac{a+b}{2} \]
\[ \Var{X} = \frac{(b-a)^2}{12} \]

### Exponential distribution

For \(x \geq 0\),

\[ f(x) = \lambda e^{-\lambda x} \]
\[ \E{X} = \frac{1}{\lambda} \]
\[ \Var{X} = \frac{1}{\lambda^2} \]

### Normal distribution

\[ f(x) = \frac{1}{\sqrt{2 \pi \sigma^2}} e^{-\frac{1}{2 \sigma^2} (x - \mu)^2} \]
\[ \E{X} = \mu \]
\[ \Var{X} = \sigma^2 \]

### Gamma distribution

For \(x \geq 0\),

\[ f(x) = \frac{1}{\Gamma(\alpha)} \beta^\alpha x^{\alpha-1} e^{-\beta x} \]
\[ \E{X} = \frac{\alpha}{\beta} \]
\[ \Var{X} = \frac{\alpha}{\beta^2} \]
\[ \Gamma(\alpha) = \int_0^\infty x^{\alpha-1} e^{-x} \, dx \]

#### Properties of the Gamma function

For \( \alpha = 1 \),

\[ \Gamma(1) = 1 \]

For \( \alpha > 1 \),

\[ \Gamma(\alpha) = (\alpha - 1) \Gamma(\alpha - 1) \]

For integer \(n \geq 1\),

\[ \Gamma(n) = (n-1)! \]
