---
title: Maxwell's equations
description: List of Maxwell's equations for electromagnetism.
published: 2024-11-15
toc: true
include-math: true
include-bibliography-stylesheet: true
---

Reference for the multiple forms of Maxwell's equations for electromagnetism.

## Differential form {#differential}

\begin{align*}
\vec{\nabla} \cdot \vec{E} & = \rho \\
\vec{\nabla} \cdot \vec{B} & = 0 \\
\vec{\nabla} \times \vec{E} & = - \frac{\partial \vec{B}}{\partial t} \\
\vec{\nabla} \times \vec{B} & = \mu_0 \epsilon_0 \frac{\partial \vec{E}}{\partial t} + \mu_0 \vec{j} \\
\end{align*}

## Integral form {#integral}

\begin{align*}
\oint \vec{E} \cdot d\vec{A} & = q \\
\oint \vec{B} \cdot d\vec{A} & = 0 \\
\oint \vec{E} \cdot d\vec{s} & = - \frac{d \Phi_B}{dt} \\
\oint \vec{B} \cdot d\vec{s} & = \mu_0 \epsilon_0 \frac{d \Phi_E}{dt} + \mu_0 j
\end{align*}

## Glossary {#glossary}

 symbol        description
------------- ----------------------
 \(\vec{E}\)   electric field
 \(\vec{B}\)   magnetic field
 \(\Phi_E\)    electric field flux
 \(\Phi_B\)    magnetic field flux
 \(q\)         charge
 \(\rho\)      charge density
 \(\vec{j}\)   displacement current

## References {#refs}

---
nocite: '@susskind2017, @walker2021'
---
