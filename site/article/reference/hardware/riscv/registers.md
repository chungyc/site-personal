---
title: RISC-V Registers
description: Registers in the RISC-V instruction set architecture.
published: 2023-03-10
updated: 2023-03-17
toc: true
include-bibliography-stylesheet: true
---

---
nocite: "@riscv:user-level"
---

## Integer registers

### General purpose

Register  ABI name  Convention                                          Saver
--------- --------- --------------------------------------------------- --------
`x0`      `zero`    Hard-wired zero
`x1`      `ra`      Return address                                      caller
`x2`      `sp`      Stack pointer                                       caller
`x3`      `gp`      Global pointer
`x4`      `tp`      Thread pointer
`x5`      `t0`      Temporary or alternate link register                caller
`x6`      `t1`      Temporary                                           caller
`x7`      `t2`      Temporary                                           caller
`x8`      `s0`/`fp` Saved register or frame pointer                     callee
`x9`      `s1`      Saved register                                      callee
`x10`     `a0`      Function argument or return value                   caller
`x11`     `a1`      Function argument or return value                   caller
`x12`     `a2`      Function argument                                   caller
`x13`     `a3`      Function argument                                   caller
`x14`     `a4`      Function argument                                   caller
`x15`     `a5`      Function argument                                   caller
`x16`     `a6`      Function argument                                   caller
`x17`     `a7`      Function argument                                   caller
`x18`     `s2`      Saved register                                      callee
`x19`     `s3`      Saved register                                      callee
`x20`     `s4`      Saved register                                      callee
`x21`     `s5`      Saved register                                      callee
`x22`     `s6`      Saved register                                      callee
`x23`     `s7`      Saved register                                      callee
`x24`     `s8`      Saved register                                      callee
`x25`     `s9`      Saved register                                      callee
`x26`     `s10`     Saved register                                      callee
`x27`     `s11`     Saved register                                      callee
`x28`     `t3`      Temporary                                           caller
`x29`     `t4`      Temporary                                           caller
`x30`     `t5`      Temporary                                           caller
`x31`     `t6`      Temporary                                           caller

#### Standard calling convention

Hardware may treat these specially for better performance.

`x1`
:   Holds return address for the call

`x2`
:   Stack pointer

`x5`
:   Alternate link register

The optional compressed instruction format depends on these conventions for `x1` and `x2`.

#### Notes

Writes to `x0` are always discarded, and reads from `x0` always returns 0.

### Special purpose

`pc`
:   Program counter

## See also

*   [RISC-V Notes](./)

## References
