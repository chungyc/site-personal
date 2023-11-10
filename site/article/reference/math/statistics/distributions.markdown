---
title: Distributions in Statistics
published: 2023-02-16
updated: 2023-11-09
description: Basic reference on distributions in statistics.
toc: true
include-math: true
---

\newcommand{\E}[1]{\mathrm{E}[#1]}
\newcommand{\Var}[1]{\mathrm{Var}[#1]}

Basic reference on distributions used in statistics.

## Discrete distributions

### Bernoulli distribution

Single trial whose outcome can be success or failure.

\[ X \sim \mathrm{Bernoulli}(p) \]
\[ P(X = n) = \begin{cases}
                1-p & \text { for } n = 0 \\
                p & \text{ for } n = 1
              \end{cases}
\]
\[ \E{X} = p \]
\[ \Var{X} = p(1-p) \]
\[ M_X(t) = 1 - p + pe^t \]

### Geometric distribution

Number of trials until first success.

\[ X \sim \mathrm{geom}(p) \]
\[ P(X = n) = (1-p)^{n-1}p \quad \text{ for } n=1,2,\ldots \]
\[ \E{X} = \frac{1-p}{p} \]
\[ \Var{X} = \frac{1-p}{p^2} \]
\[ M_X(t) = \frac{p}{1-(1-p)e^t} \quad \text{ for } t < -\ln(1-p) \]

### Binomial distribution

Number of successes in \(n\) trials.

\[ X \sim \mathrm{bin}(n,p) \]
\[ P(X = k) = {n \choose k} p^x (1-p)^{n-x} \quad \text{ for } 0 \leq k \leq n \]
\[ \E{X} = np \]
\[ \Var{X} = np(1-p) \]
\[ M_X(t) = (1 - p + p e^t)^n \]

### Poisson distribution

Distribution of rare events in a large population.

\[ X \sim \mathrm{Poisson}(\lambda) \]
\[ P(X = n) = \frac{e^{-\lambda} \lambda^n}{n!} \quad \text{ for  } n = 0, 1, 2, \ldots \]
\[ \E{X} = \lambda \]
\[ \Var{X} = \lambda \]
\[ M_X(t) = e^{\lambda (e^t - 1)} \]

## Continuous distributions

### Uniform distribution

\[ X \sim \mathrm{unif}(a,b) \]
\[ f(x) = \frac{1}{b-a} \quad \text{ for } a \leq x \leq b \]
\[ \E{X} = \frac{a+b}{2} \]
\[ \Var{X} = \frac{(b-a)^2}{12} \]
\[ M_X(t) = \frac{e^{bt} - e^{at}}{(b-a)t} \]

### Exponential distribution

\[ X \sim \mathrm{exp}(\lambda) \]
\[ f(x) = \lambda e^{-\lambda x} \quad \text{ for } x \geq 0 \]
\[ \E{X} = \frac{1}{\lambda} \]
\[ \Var{X} = \frac{1}{\lambda^2} \]
\[ M_X(t) = \frac{\lambda}{\lambda - t} \quad \text{ for } t < \lambda \]

### Normal distribution

\[ X \sim N(\mu, \sigma^2) \]
\[ f(x) = \frac{1}{\sqrt{2 \pi \sigma^2}} e^{-\frac{1}{2 \sigma^2} (x - \mu)^2} \]
\[ \E{X} = \mu \]
\[ \Var{X} = \sigma^2 \]
\[ M_X(t) = e^{\mu t + \frac{1}{2} \sigma^2 t^2} \]

### Gamma distribution

\[ X \sim \Gamma(\alpha,\beta) \]
\[ f(x) = \frac{1}{\Gamma(\alpha)} \beta^\alpha x^{\alpha-1} e^{-\beta x} \quad \text{ for } x \geq 0 \]
\[ \Gamma(\alpha) = \int_0^\infty x^{\alpha-1} e^{-x} \, dx \]
\[ \E{X} = \frac{\alpha}{\beta} \]
\[ \Var{X} = \frac{\alpha}{\beta^2} \]
\[ M_X(t) = \left( \frac{\beta}{\beta - t} \right)^\alpha \quad \text{ for } t < \beta \]

#### Properties of the Gamma function

For \( \alpha = 1 \),

\[ \Gamma(1) = 1 \]

For \( \alpha > 1 \),

\[ \Gamma(\alpha) = (\alpha - 1) \Gamma(\alpha - 1) \]

For integer \(n \geq 1\),

\[ \Gamma(n) = (n-1)! \]

## See also

* [General properties in statistics](./)
