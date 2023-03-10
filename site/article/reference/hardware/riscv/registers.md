---
title: RISC-V Registers
description: Registers in the RISC-V instruction set architecture.
published: 2023-03-10
extra-stylesheet: /css/bibliography.css
---

---
nocite: "@riscv:user-level"
---

## Integer registers

### General purpose

`x0`, `x1`, `x2`, `x3`, `x4`, `x5`, `x6`, `x7`,
`x8`, `x9`, `x10`, `x11`, `x12`, `x13`, `x14`, `x15`,
`x16`, `x17`, `x18`, `x19`, `x20`, `x21`, `x22`, `x23`,
`x24`, `x25`, `x26`, `x27`, `x28`, `x29`, `x30`, `x31`

#### Standard calling convention

`x1`
:   Holds return address for the call

`x2`
:   Stack pointer

`x5`
:   Alternate link register

#### Notes

*   Writes to `x0` are always discarded, and reads from `x0` always returns 0.

### Special purpose

`pc`
:   Program counter

## References
