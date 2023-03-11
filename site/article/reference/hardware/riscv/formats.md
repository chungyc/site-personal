---
title: RISC-V Instruction Formats
description: Instruction formats for the RISC-V instruction set architecture.
published: 2023-03-10
toc: true
extra-stylesheet: /css/bibliography.css
---

---
nocite: "@riscv:user-level"
---

Instructions are all 32 bits in the base instruction set.
They must be aligned on a four-byte boundary in memory.
A branch or jump instruction with a misaligned target address
will generate an "instruction address misaligned" exception.

## Variants

### R

--------- --------
 [0:6]    `opcode` 
 [7:11]   `rd`
 [12:14]  `funct3`
 [15:19]  `rs1`
 [20:24]  `rs2`
 [25:31]  `funct7` 
--------- --------

### I

--------- -------------
 [0:6]    `opcode` 
 [7:11]   `rd`
 [12:14]  `funct3`
 [15:19]  `rs1`
 [20:31]  `imm`[0:11]
--------- -------------

### S

--------- -------------
 [0:6]    `opcode` 
 [7:11]   `imm`[0:4]
 [12:14]  `funct3`
 [15:19]  `rs1`
 [20:24]  `rs2`
 [25:31]  `imm`[5:11]
--------- -------------

#### B

A variant of the S format.

--------- -------------
 [0:6]    `opcode` 
 [7]      `imm`[11]
 [8:11]   `imm`[1:4]
 [12:14]  `funct3`
 [15:19]  `rs1`
 [20:24]  `rs2`
 [25:30]  `imm`[5:10]
 [31]     `imm`[12]
--------- -------------

### U

--------- -------------
 [0:6]    `opcode` 
 [7:11]   `rd`
 [12:31]  `imm`[12:31]
--------- -------------

#### J

A variant of the U format.

--------- -------------
 [0:6]    `opcode` 
 [7:11]   `rd`
 [12:19]  `imm`[12:19]
 [20]     `imm`[11]
 [21:30]  `imm`[1:10]
 [31]     `imm`[20]
--------- -------------

### Notes

*   All instructions have `opcode` in bits [0:6]
*   `rd` will always be in bits [7:11]
*   `rs1` will always be in bits [15:19]
*   `rs2` will always be in bits [20:24]
*   Immediates will always be sign-extended
    *   Sign bits will always be in bit [31]

## References
