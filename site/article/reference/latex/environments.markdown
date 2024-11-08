---
title: LaTeX environments
description: Reference of LaTeX symbols useful to me.
published: 2024-11-08
toc: true
include-math: true
include-syntax-stylesheet: true
---

Basic reference of LaTeX environments that are useful to me.
For now, these are limited to those that I have found useful for this web site.

## `align*` {#align}

```latex
\begin{align*}
y & = 2x + 5 \\
y & = 3x - 1
\end{align*}
```

\begin{align*}
y & = 2x + 5 \\
y & = 3x - 1
\end{align*}

## `cases` {#cases}

```latex
\[
x_{n+1} = \begin{cases}
            3 x_n + 1 & \text{if $x_n$ is odd} \\
            \frac{x_n}{2} & \text{if $x_n$ is even}
          \end{cases}
\]
```

\[
x_{n+1} = \begin{cases}
            3 x_n + 1 & \text{if $x_n$ is odd} \\
            \frac{x_n}{2} & \text{if $x_n$ is even}
          \end{cases}
\]
