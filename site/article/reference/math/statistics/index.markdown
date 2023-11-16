---
title: Statistics
description: Notes on general definitions and properties in statistics.
published: 2023-11-07
updated: 2023-11-16
include-math: true
include-bibliography-stylesheet: true
toc: true
---

\newcommand{\E}[1]{\mathrm{E}[#1]}
\newcommand{\Var}[1]{\mathrm{Var}[#1]}
\newcommand{\Cov}[2]{\mathrm{Cov}(#1,#2)}
\newcommand{\Corr}[2]{\mathrm{Corr}(#1,#2)}

Basic reference on basic concepts and properties in statistics.

Based on notes taken during a course [@coursera:statistical-inference-for-estimation].

## Expectation

\[ \E{X} = \sum x P(X = x) \]

\[ \E{X} = \int x f(x) \, dx \]

Expectation is a linear operator:

\[ \E{aX + bY} = a \E{X} + b \E{Y} \]

If $X$ and $Y$ are independent:

\[ \E{XY} = \E{X} \E{Y} \]

## Variance

\[ \Var{X} = \sigma^2 = \E{(X - \E{X})^2} \]

Standard deviation $\sigma$ is defined as the square root of the variance $\sigma^2$.

Other properties:

\[ \Var{X} = \E{X^2} - \E{X}^2 \]

\[ \Var{aX} = a^2 \Var{X} \]

## Covariance

\[ \Cov{X}{Y} = \sigma_{X,Y} = E[(X-\E{X})(Y-\E{Y})] \]

\[ \Cov{X}{Y} = \E{XY} - \E{X} \E{Y} \]

\[ \Cov{X}{X} = \Var{X} \]

\[ \Var{X+Y} = \Var{X} + 2 \Cov{X}{Y} + \Var{Y} \]

## Correlation

\[ \Corr{X}{Y} = \rho_{X,Y} = \frac{\Cov{X}{Y}}{\sqrt{\Var{X} \Var{Y}}} \]

If $X$ and $Y$ are independent, then they are _uncorrelated_:

\[ \Corr{X}{Y} = 0 \]

## Transformation

If $f_X(x)$ is a probability density function for $X$,
then for $Y=g(X)$, where $g$ is invertible:

\[ f_Y(y) = f_X(g^{-1}(y)) \cdot \left| \frac{d}{dy} g^{-1}(y) \right| \]

## Estimators

An _estimator_ is a random variable estimating the true value of a parameter.

For example, the mean of a random sample $\hat{\theta} = \overline{X}$
is an estimator for the true mean $\E{X}$.

An estimator $\hat{\theta}$ estimating $\theta$ is _unbiased_
if $\E{\hat{\theta}} = \theta$.

\[ \E{\overline{X}} = \E{X} \]

\[ \E{\Var{\overline{X}}} = \frac{\Var{X}}{n^2} \]

The _bias_ of $\hat{\theta}$ is

\[ \mathrm{B}(\hat{\theta}) = \E{\hat{\theta}} - \theta \]

The _mean squared error_ is

\[ \mathrm{MSE}(\hat{\theta}) = \E{(\hat{\theta} - \theta)^2} =
   \Var{\hat{\theta}} + \mathrm{B}(\hat{\theta})^2 \]

For two unbiased estimators $\hat{\theta}_1$ and $\hat{\theta}_2$,
$\hat{\theta}_1$ is more _efficient_ than $\hat{\theta}_2$ if

\[ \Var{\hat{\theta}_1} < \Var{\hat{\theta}_2} \]

### Method of moments estimation

Assuming a particular distribution with unknown parameters,
pretend the sample moments, i.e., $\E{\overline{X}}, \E{\overline{X}^2}, \ldots$
are equal to the true moments, and solve for the distribution parameters.

### Maximum likelihood estimation

Assuming a particular distribution with unknown parameters,
the _maximum likelihood estimator_ is the set of parameters which result
in the highest probability for the observed samples.[^not-bayesian]

[^not-bayesian]: Note that the maximum likelihood estimator may not
  be the most _probable_ set of parameters.  A large set of statisticians
  have an aversion to using Bayes' theorem.

_Likelihood_ is proportional to the joint probability mass function or density function,
assuming a particular distribution with unknown parameters.

#### Invariance property

If $\tau$ is an invertible function, the maximum likelihood estimator for $\tau(\theta)$
is $\tau(\hat{\theta})$, where $\hat{\theta}$ is the maximum likelhood estimator for $\theta$.

## Moment generating functions

\[ M_X(t) = \E{e^{tX}} = \int_{-\infty}^\infty e^{tx} f_X(x) \, dx \]

For independent $X_1$, ..., $X_n$ and $Y = \sum_{k=1}^n X_k$,

\[ M_Y(t) = \prod_{k=1}^n M_X(t) \]

If two probability distributions have the same moment generating function,
they are the same distribution.

## Cramér-Rao lower bound

\[ \Var{\tau(\theta)} \geq
   \frac{(\tau'(\theta))^2}{\mathrm{E} \left [
     \left( \frac{\partial}{\partial \theta} \ln f(\vec{x}; \theta) \right)^2
   \right] }
\]

The lower bound holds if

\[ \frac{\partial}{\partial \theta} \int f(\vec{x}; \theta) \, dx =
   \int \frac{\partial}{\partial \theta} f(\vec{x}; \theta) \, dx \]

\[ \frac{\partial}{\partial \theta} \ln f(\vec{x}; \theta) \quad \text{exists} \]

\[ 0 < \mathrm{E} \left[ \left( \frac{\partial}{\partial \theta} \ln f(\vec{x}; \theta) \right)^2 \right] < \infty \]

### Fisher information properties

\[ \mathrm{E}\left[ \frac{\partial}{\partial \theta} \ln f(\vec{x}; \theta) \right] = 0 \]

\[ \mathrm{E}\left[ \left( \frac{\partial}{\partial\theta} \ln f(\vec{x};\theta) \right)^2 \right]
 = -\mathrm{E}\left[ \frac{\partial^2}{\partial\theta^2} \ln f(\vec{x};\theta) \right]
\]

If $Y=(X_1, \ldots, X_n)$ is a tuple of independent and identically distributed random variables,

\[ \mathrm{E}\left[ \left( \frac{\partial}{\partial\theta} \ln f_Y(\vec{y};\theta) \right)^2 \right]
 = n \mathrm{E}\left[ \left( \frac{\partial}{\partial\theta} \ln f_X(\vec{x};\theta) \right)^2 \right]
\]

## See also

* [Common distributions](distributions)

## References
