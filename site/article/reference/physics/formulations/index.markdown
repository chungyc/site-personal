---
title: Mathematical formulations for physics
description: Personal reference for the various formulations of physics such as Newtonian, Lagrangian, and Hamiltonian.
published: 2024-01-31
updated: 2024-02-08
include-math: true
include-bibliography-stylesheet: true
toc: true
---

Personal reference on the various ways to mathematically formulate physics.

In all of the following, \(v_i = \frac{x_i}{dt}\),
and \(x_i\) is not necessarily a Cartesian coordinate except in the Newtonian formulation.

## Newtonian {#newtonian}

\[ \frac{\mathbf{p}}{m} = \frac{d\mathbf{x}}{dt} \]
\[ \mathbf{F} = \frac{d\mathbf{p}}{dt} \]
\[ \mathbf{F} = - \nabla V \]

## Lagrangian {#lagrangian}

For a Lagrangian \(\mathcal{L}\), the action \(\mathcal{A}\) is

\[ \mathcal{A} = \int_{t_0}^{t_1} \mathcal{L\left(\mathbf{x}(t), \mathbf{v}(t)\right)} \, dt \]

The trajectory satisfies the principle of least action or stationary action.

\[ \delta \mathcal{A} = 0\]

### Euler-Lagrange equation {#euler-lagrange}

\[ \frac{d}{dt} \frac{\partial \mathcal{L}}{\partial v_i} = \frac{\partial \mathcal{L}}{\partial x_i} \]

### Generalized momentum {#lagrangian-momentum}

\[ p_i = \frac{\partial \mathcal{L}}{\partial v_i} \]

### Classical Lagrangian {#classical-lagrangian}

\[ \mathcal{L} = T - V = \frac{1}{2} m v^2 - V \]

### Symmetry and conservation {#symmetry-lagrangian}

For a symmetry which leaves the Lagrangian invariant, there is a conserved quantity.

## Hamiltonian {#hamiltonian}

\[ H = \sum p_i v_i - \mathcal{L} = T + V = E \]

### Equations of motion {#hamiltonian-motion-equations}

\[ \frac{dp_i}{dt} = - \frac{\partial H}{\partial x_i} \]
\[ \frac{dx_i}{dt} = \frac{\partial H}{\partial p_i} \]

### Gibbs-Liouville theorem {#incompressibility}

For \(\mathbf{v} = (x_1, \ldots, x_n, p_1, \ldots, p_n)\) in phase space,

\[ \nabla \cdot \mathbf{v} = 0 \]

### Poisson bracket {#poisson-bracket}

\[ \{F,G\} = \sum_i \left(
      \frac{\partial F}{\partial q_i} \frac{\partial G}{\partial p_i} -
      \frac{\partial F}{\partial p_i} \frac{\partial G}{\partial q_i}
   \right)
\]

With the Hamiltonian $H$,

\[ \frac{dF}{dt} = \{F,H\} \]

The equations of motions for a Hamiltonian can be expressed with the Poisson bracket.

\[ \frac{d q_k}{dt} = \{q_k,H\} \]
\[ \frac{d p_k}{dt} = \{p_k,H\} \]

#### Basic properties {#poisson-bracket-axioms}

\[ \{A,C\} = -\{C,A\} \]
\[ \{cA,C\} = c\{A,C\} \]
\[ \{A+B,C\} = \{A,C\} + \{B,C\} \]
\[ \{AB,C\} = A\{B,C\} + B\{A,C\} \]
\[ \{q_i,q_j\} = 0 \]
\[ \{p_i,p_j\} = 0 \]
\[ \{q_i,p_j\} = \delta_{ij} \]

#### Other properties {#poisson-bracket-properties}

\[ \{f,p_i\} = \frac{\partial f}{\partial q_i} \]
\[ \{f,q_i\} = -\frac{\partial f}{\partial p_i} \]

## References

---
nocite: '@susskind2013, @baggott2020'
---
