---
title: Confidence intervals
description: Personal notes on confidence intervals in statistics.
published: 2023-12-16
updated: 2024-01-09
include-math: true
include-bibliography-stylesheet: true
toc: true
---

Personal notes on confidence intervals.

Based on notes taken during a course [@coursera:statistical-inference-for-estimation].

## Confidence interval {#confidence-interval}

> Confidence interval for mean $\mu$ is $(a,b)$ with confidence $p$

means the same thing as

> The procedure from which the interval $(a,b)$ was sampled returns
> an interval which contains mean $\mu$ with probability $p$

## For normal distributions {#normal}

### With known variance {#normal-known-variance}

When $X \sim N(\mu, \sigma^2)$ and variance $\sigma^2$ is known,
the $1-\alpha$ confidence interval for $\mu$ is

\[ \overline{X} \pm z_\frac{\alpha}{2} \frac{\sigma}{\sqrt{n}} \]

### With unknown variance {#normal-unknown-variance}

For sample mean $\overline{X}$, sample variance $S^2$, and sample count $n$,
$\frac{\overline{X} - \mu}{\frac{S}{\sqrt{n}}}$ has a [$t$-distribution].

\[ \frac{\overline{X} - \mu}{\frac{S}{\sqrt{n}}} \sim t(n-1) \]

The $1-\alpha$ confidence interval is

\[ \overline{X} \pm t_{\frac{\alpha}{2}, n-1} \frac{S}{\sqrt{n}} \]

[$t$-distribution]: /article/reference/math/statistics/distributions#t-dist

## For any distribution with large population {#large-population}

For any distribution, when sample count is large enough for the distribution of $\overline{X}$
to approximate a normal distribution as per the [central limit theorem],
the confidence interval for normal distributions is a reasonable approximation.

[central limit theorem]: /article/reference/math/probability#central-limit-theorem

## Difference of means {#mean-difference}

### For normal distributions {#mean-difference-normal}

#### With known variances {#mean-difference-normal-known-vars}

\[ \left( \overline{X_1} - \overline{X_2} \right) \pm
   z_\frac{\alpha}{2} \sqrt{\frac{\sigma_1^2}{n_1} + \frac{\sigma_2^2}{n_2}} \]

#### With same unknown variance {#mean-difference-normal-same-var}

\[ \left( \overline{X_1} - \overline{X_2} \right) \pm
   t_{\frac{\alpha}{2}, n_1 + n_2 - 2}
   \sqrt{S_p^2 \left( \frac{1}{n_1} + \frac{1}{n_2} \right)} \]


##### Pooled variance {#pooled-variance}

\[ S_p^2 = \frac{(n_1 - 1) S_1^2 + (n_2 - 1) S_2^2}{n_1 + n_2 - 2} \]

####  With unknown variances {#mean-difference-normal-unknown-vars}

Obtaining a confidence interval for the difference of means from
normal distributions with separate unknown variances is known as the Behrens-Fisher problem.
It can be approximated with Welch's approximation:

\[ T = \frac{(\overline{X_1} - \overline{X_2}) - (\mu_1 - \mu_2)}
            {\sqrt{\frac{S_1^2}{n_1} + \frac{S_2^2}{n_2}}}
   \approx t(\nu) \]

\[ \nu = \frac{\left( \frac{S_1^2}{n_1} + \frac{S_2^2}{n_2} \right)^2}
              {\frac{\left( \frac{S_1^2}{n_1} \right)^2}{n_1 - 1} +
               \frac{\left( \frac{S_2^2}{n_2} \right)^2}{n_2 - 1} } \]

## Proportion of variances {#variance-proportion}

If $X_1 \sim N(\mu_1,\sigma_1^2)$ and $X_2 \sim N(\mu_2,\sigma_2^2)$,
then the following has the [$F$-distribution](../distributions#f-dist).

\[ \frac{\sigma_2^2}{\sigma_1^2} \cdot \frac{S_1^2}{S_2^2} \sim F(n_1-1, n_2-1) \]

$\sigma_1$ and $\sigma_2$ are the true variances,
while $S_1$ and $S_2$ are the sample variances.

## See also

*   [General properties in statistics](../)

## References
