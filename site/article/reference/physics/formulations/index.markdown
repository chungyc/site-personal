---
title: Mathematical formulations for physics
description: Personal reference for the various formulations of physics such as Newtonian, Lagrangian, and Hamiltonian.
published: 2024-01-31
include-math: true
include-bibliography-stylesheet: true
toc: true
---

Personal reference on the various ways to mathematically formulate physics.

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

If \(v_i = \frac{dx_i}{dt}\), then the following is the generalized momentum conjugate to \(x_i\):

\[ p_i = \frac{\partial \mathcal{L}}{\partial v_i} \]

### Classical Lagrangian {#classical-lagrangian}

\[ \mathcal{L} = T - V = \frac{1}{2} m v^2 - V \]

## Hamiltonian {#hamiltonian}

\[ H = E = T + V \]

### Equations of motion {#hamiltonian-motion-equations}

\[ \frac{dp_i}{dt} = - \frac{\partial H}{\partial x_i} \]
\[ \frac{dx_i}{dt} = \frac{\partial H}{\partial p_i} \]

## References

---
nocite: '@susskind2013'
---
