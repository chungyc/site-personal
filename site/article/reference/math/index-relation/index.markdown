---
title: Index relation functions
description: Personal reference on functions which relate indexes, such as the Kronecker delta or the Levi-Civita symbols.
published: 2024-02-07
include-math: true
toc: true
---

Personal reference on functions whose values depend on the relation between indexes.
"Index relation functions" is not a term in actual use by anyone else,
but there does not seem to be a word in widespread use for functions like these,
where the function values relate indexes against each other.

## Kronecker delta {#kronecker}

\[
\delta_{ij} = \begin{cases}
                1 & \text { for } i = j \\
                0 & \text { for } i \neq j
              \end{cases}
\]

## Levi-Civita symbol {#levi-civita}

\[
\epsilon_{1 2 \ldots n} = 1
\]
\[
\epsilon_{\ldots i_p \ldots i_q \ldots} = -\epsilon_{\ldots i_q \ldots i_p \ldots}
\]

\[
\epsilon_{ijk} = \begin{cases}
                   0 & \text { for } i=j \vee j=k \vee k=i \\
                   1 & \text { for } (i,j,k) \in \{ (1,2,3), (2,3,1), (3,1,2) \} \\
                   -1 & \text { for } (i,j,k) \in \{ (3,2,1), (2,1,3), (1,3,2) \}
                 \end{cases}
\]
