---
title: RISC-V RV32I Instructions
description: RV32I base instructions for the RISC-V instruction set architecture.
published: 2023-03-17
toc: true
extra-stylesheet: /css/bibliography.css
---

---
nocite: "@riscv:user-level"
---

## Integer instructions

ADDI
:   Add immediate.  I type.

    MV assembler pseudo-instruction is implemented in terms of ADDI.
    I.e., `mv rd, rs` is actually `addi rd, rs, 0`.
	
	NOP assembler psuedo-instruction is implemted in terms of ADDI.
	I.e., `nop` is actually `addi x0, x0, 0`.

SLTI
:   Set less than immediate.  I type.

    If `rs1` is less than the immediate value, set `rd` to 1.
	Otherwise, `rd` is set to 0.  The values are compared as signed numbers.

SLTIU
:   Set less than immediate, unsigned.  I type.

    Same as SLTI, except values are compared as unsigned numbers.
    The immediate value is still sign-extended.
	
	SEQZ assembler pseudo-instruction is implemented in terms of SLTIU.
	I.e., `seqz rd, rs` is actually `sltiu rd, rs, 1`.

ANDI
:   Bitwise AND.  I type.

ORI
:   Bitwise OR.  I type.

XORI
:   Bitwise XOR.  I type.

    NOT assembler pseudo-instruction is implemented in terms of XORI.
	I.e., `not rd, rs` is actually `xori rd, rs, -1`.

SLLI
:   Logical left shift.  Specialized I type.

    The shift amount is encoded in the immediate value,
	but the entire immediate value is not the shift amount.
    The shift amount is in the lower 5 bits of the immediate value.
	The rest of the immediate value encodes the type of shift.
	
	0000000 encodes logical shift.

SRLI
:   Logical right shift.  Specialized I type.

    Immediate value encoding is the same as SLLI.

SRAI
:   Arithmetic right shift.  Specialized I type.

    Immediate value encoding is similar to SLLI.
	0100000 encodes arithmetic shift.

LUI
:   Load upper immediate.  U type.

AUIPC
:   Add upper immediate to `pc`.  U type.

ADD
:   Addition; `rs1 + rs2`.  R type.

SUB
:   Subtraction; `rs1 - rs2`.  R type.

SLT
:   Signed compare.  R type.

    Set `rd` to 1 if `rs1 < rs2`, otherwise set to 0.

SLU
:   Unsigned compare.  R type.

    Set `rd` to 1 if `rs1 < rs2`, otherwise set to 0.

	SNEZ assembler psuedo-instruction is implemented in terms of SLU.
	I.e., `snez rd, rs` is actually `slu rd, x0, rs`.

SLL
:   Logical left shift.  R type.

    Shifts `rs1` by shift amount held in lower 5 bits of `rs2`.

SRL
:   Logical right shift.  R type.

    Shifts `rs1` by shift amount held in lower 5 bits of `rs2`.

SRA
:   Arithmetic right shift.  R type.

    Shifts `rs1` by shift amount held in lower 5 bits of `rs2`.

## Control instructions

## Load and store instructions

## Memory ordering instructions

## Environment call and breakpoints

## Hint instructions

## See also

*   [Instruction formats](../formats.md)

## References
