---
title: Distributions in Statistics
published: 2023-02-16
updated: 2024-01-09
description: Basic reference on distributions in statistics.
toc: true
include-math: true
---

\newcommand{\E}[1]{\mathrm{E}[#1]}
\newcommand{\Var}[1]{\mathrm{Var}[#1]}

Basic reference on distributions used in statistics.

## Discrete distributions {#discrete}

### Bernoulli distribution {#bernoulli}

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

### Geometric distribution {#geometric}

Number of trials until first success.

\[ X \sim \mathrm{geom}(p) \]
\[ P(X = n) = (1-p)^{n-1}p \quad \text{ for } n=1,2,\ldots \]
\[ \E{X} = \frac{1-p}{p} \]
\[ \Var{X} = \frac{1-p}{p^2} \]
\[ M_X(t) = \frac{p}{1-(1-p)e^t} \quad \text{ for } t < -\ln(1-p) \]

### Binomial distribution {#binomial}

Number of successes in \(n\) trials.

\[ X \sim \mathrm{bin}(n,p) \]
\[ P(X = k) = {n \choose k} p^x (1-p)^{n-x} \quad \text{ for } 0 \leq k \leq n \]
\[ \E{X} = np \]
\[ \Var{X} = np(1-p) \]
\[ M_X(t) = (1 - p + p e^t)^n \]

### Poisson distribution {#poisson}

Distribution of rare events in a large population.

\[ X \sim \mathrm{Poisson}(\lambda) \]
\[ P(X = n) = \frac{e^{-\lambda} \lambda^n}{n!} \quad \text{ for  } n = 0, 1, 2, \ldots \]
\[ \E{X} = \lambda \]
\[ \Var{X} = \lambda \]
\[ M_X(t) = e^{\lambda (e^t - 1)} \]

## Continuous distributions {#continuous}

### Uniform distribution {#uniform}

\[ X \sim \mathrm{unif}(a,b) \]
\[ f(x) = \frac{1}{b-a} \quad \text{ for } a \leq x \leq b \]
\[ \E{X} = \frac{a+b}{2} \]
\[ \Var{X} = \frac{(b-a)^2}{12} \]
\[ M_X(t) = \frac{e^{bt} - e^{at}}{(b-a)t} \]

### Exponential distribution {#exponential}

\[ X \sim \mathrm{exp}(\lambda) \]
\[ f(x) = \lambda e^{-\lambda x} \quad \text{ for } x \geq 0 \]
\[ \E{X} = \frac{1}{\lambda} \]
\[ \Var{X} = \frac{1}{\lambda^2} \]
\[ M_X(t) = \frac{\lambda}{\lambda - t} \quad \text{ for } t < \lambda \]

### Normal distribution {#normal}

\[ X \sim N(\mu, \sigma^2) \]
\[ f(x) = \frac{1}{\sqrt{2 \pi \sigma^2}} e^{-\frac{1}{2 \sigma^2} (x - \mu)^2} \]
\[ \E{X} = \mu \]
\[ \Var{X} = \sigma^2 \]
\[ M_X(t) = e^{\mu t + \frac{1}{2} \sigma^2 t^2} \]

#### Properties {#normal-properties}

Any linear combination of normal distributions will also be a normal distribution.

### Gamma distribution {#gamma}

\[ X \sim \Gamma(\alpha,\beta) \]
\[ f(x) = \frac{1}{\Gamma(\alpha)} \beta^\alpha x^{\alpha-1} e^{-\beta x} \quad \text{ for } x \geq 0 \]
\[ \Gamma(\alpha) = \int_0^\infty x^{\alpha-1} e^{-x} \, dx \]
\[ \E{X} = \frac{\alpha}{\beta} \]
\[ \Var{X} = \frac{\alpha}{\beta^2} \]
\[ M_X(t) = \left( \frac{\beta}{\beta - t} \right)^\alpha \quad \text{ for } t < \beta \]

#### Properties {#gamma-properties}

For \( \alpha = 1 \),

\[ \Gamma(1) = 1 \]

For \( \alpha > 1 \),

\[ \Gamma(\alpha) = (\alpha - 1) \Gamma(\alpha - 1) \]

For integer \(n \geq 1\),

\[ \Gamma(n) = (n-1)! \]

### Chi-squared distribution {#chi-squared}

A chi-squared distribution is defined in terms of a gamma distribution.
If for random variable $X$, \( X \sim \Gamma(\frac{n}{2}, \frac{1}{2}) \), then

\[ X \sim \chi^2(n) \]

#### Properties {#chi-squared-properties}

For $X_i \sim \chi^2(n_i)$ and $Y=\sum_{i=1}^n X_i$,

\[ Y \sim \chi^2 \left( \sum_{i=1}^n n_i \right) \]

For $X \sim N(0,1)$ and $Y=X^2$,

\[ Y \sim \chi^2(1) \]

For sample variance $S^2$ of $n$ random samples from a normal distribution $N(\mu,\sigma^2)$,

\[ \frac{(n-1) S^2}{\sigma^2} \sim \chi^2(n-1) \]

The sample variance $S^2$ is defined by

\[ S^2 = \frac{1}{n-1} \sum_{i=1}^{n} X_i \]

### $t$-distribution {#t-dist}

For $Z \sim N(0,1)$ and $W \sim \chi^2(n)$,
the following random variable $T$ has the $t$-distribution.

\[ T=\frac{Z}{\sqrt{\frac{W}{n}}} \]

$T$ is said to have a $t$-distribution with $n$ degrees of freedom.

\[ T \sim t(n) \]
\[ f(t) = \frac{\Gamma\left(\frac{n+1}{2}\right)}{\Gamma\left(\frac{n}{2}\right)}
          \frac{1}{\sqrt{n\pi}}
          \left(1+\frac{t^2}{n}\right)^{-\frac{n+1}{2}}
\]
\[ \E{T} = 0 \]
\[ \Var{T} = \frac{n}{n-2} \]

### $F$-distribution {#f-dist}

For $X_1 \sim \chi^2(n_1)$ and $X_2 \sim \chi^2(n_2)$,
the following random variable $F$ has the $F$-distribution.

\[ F = \frac{X_1 / n_1}{X_2 / n_2} \]

\[ F \sim F(n_1-1,n_2-1) \]
\[ \E{F} = \frac{n_2}{n_2-2} \quad \text{ for } n_2 > 2 \]
\[ \Var{F} = \frac{2n_2^2(n_1+n_2-2)}{n_1(n_2-2)^2(n_2-4)} \quad \text{ for } n_2 > 2 \]

## See also

* [General properties in statistics](./)
