---
title: RISC-V Instruction Formats
description: Instruction formats for the RISC-V instruction set architecture.
published: 2023-03-10
updated: 2023-03-21
toc: true
include-bibliography-stylesheet: true
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

### B

A variant of the [S format](#s).

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

### J

A variant of the [U format](#u).

--------- -------------
 [0:6]    `opcode` 
 [7:11]   `rd`
 [12:19]  `imm`[12:19]
 [20]     `imm`[11]
 [21:30]  `imm`[1:10]
 [31]     `imm`[20]
--------- -------------

## Notes

*   All instructions have `opcode` in bits [0:6]

*   `rd` will always be in bits [7:11]

*   `rs1` will always be in bits [15:19]

*   `rs2` will always be in bits [20:24]

*   Immediates will always be sign-extended

    *   Sign bits will always be in bit [31]

*   Multiple operations can have the same opcode.

    *   Most can be distinguished using "funct" fields in the instruction.

    *   A few encode different operations in immediate values.

*   The RISC-V Instruction Set Manual [@riscv:user-level] uses mnemonics,
    instead of showing opcode or "funct" field values directly,
	in each chapter explaining individual instruction sets and extensions.
	For example, it uses "OP-IMM" or "OP" for integer instruction opcodes
	in chapter 2.

    The values of these mnemonics can be found in chapter 24,
	"RV32/64G Instruction Set Listings".

## See also

*   [RISC-V Notes](./)

## References
