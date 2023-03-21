---
title: RISC-V RV32I Instructions
description: RV32I base instructions for the RISC-V instruction set architecture.
published: 2023-03-17
updated: 2023-03-21
toc: true
include-math: true
include-bibliography-stylesheet: true
---

\newcommand{\opcode}[1]{\mathrm{opcode} = #1}
\newcommand{\funct}[2]{\mathrm{funct#1} = #2}

---
nocite: "@riscv:user-level"
---

## Integer instructions

### Register-immediate; I type

For [I type] integer register-immediate instructions, $\opcode{0010011_2}$.

ADDI
:   Add immediate.

    *   MV assembler pseudo-instruction is implemented in terms of ADDI.
        I.e., `mv rd, rs` is actually `addi rd, rs, 0`.
    
    *   NOP assembler psuedo-instruction is implemted in terms of ADDI.
        I.e., `nop` is actually `addi x0, x0, 0`.

    [I type]; $\funct3{000_2}$

SLTI
:   Set less than immediate.
    If `rs1` is less than the immediate value, set `rd` to 1.
    Otherwise, `rd` is set to 0.  The values are compared as signed numbers.

    [I type]; $\funct3{010_2}$

SLTIU
:   Set less than immediate, unsigned.
    Same as SLTI, except values are compared as unsigned numbers.
    The immediate value is still sign-extended.
    
    *   SEQZ assembler pseudo-instruction is implemented in terms of SLTIU.
        I.e., `seqz rd, rs` is actually `sltiu rd, rs, 1`.

    [I type]; $\funct3{011_2}$

ANDI
:   Bitwise AND.

    [I type]; $\funct3{111_2}$

ORI
:   Bitwise OR.

    [I type]; $\funct3{110_2}$

XORI
:   Bitwise XOR.

    *   NOT assembler pseudo-instruction is implemented in terms of XORI.
        I.e., `not rd, rs` is actually `xori rd, rs, -1`.

    [I type]; $\funct3{100_2}$

SLLI
:   Logical left shift.
    The shift amount is encoded in the immediate value,
    but the entire immediate value is not the shift amount.
    The shift amount is in the lower 5 bits of the immediate value.
    The rest of the immediate value encodes the type of shift.
    
    0000000 encodes logical shift.

    Specialized [I type]; $\funct3{001_2}$

SRLI
:   Logical right shift.
    Immediate value encoding is the same as SLLI.

    Specialized [I type]; $\funct3{101_2}$

SRAI
:   Arithmetic right shift.
    Immediate value encoding is similar to SLLI.

    0100000 encodes arithmetic shift.

    Specialized [I type]; $\funct3{101_2}$

### Register-immediate; U type

LUI
:   Load upper immediate.

    [U type]; opcode $\opcode{0110111_2}$

AUIPC
:   Add upper immediate and `pc`.

    [U type]; $\opcode{0010111_2}$

### Register-register

For [R type] integer register-register instructions, $\opcode{0110011_2}$.

ADD
:   Addition; `rs1 + rs2`.

    [R type]; $\funct3{000_2}$; $\funct7{0000000_2}$

SUB
:   Subtraction; `rs1 - rs2`.

    [R type]; $\funct3{000_2}$; $\funct7{0100000_2}$

SLT
:   Signed compare.
    Set `rd` to 1 if `rs1 < rs2`, otherwise set to 0.

    [R type]; $\funct3{010_2}$; $\funct7{0000000_2}$

SLTU
:   Unsigned compare.
    Set `rd` to 1 if `rs1 < rs2`, otherwise set to 0.

    *   SNEZ assembler psuedo-instruction is implemented in terms of SLU.
        I.e., `snez rd, rs` is actually `slu rd, x0, rs`.

    [R type]; $\funct3{011_2}$; $\funct7{0000000_2}$

AND
:   Bitwise AND.

    [R type]; $\funct3{111_2};$ $\funct7{0000000_2}$

OR
:   Bitwise OR.

    [R type]; $\funct3{110_2}$; $\funct7{0000000_2}$

XOR
:   Bitwise XOR.

    [R type]; $\funct3{100_2}$; $\funct7{0000000_2}$

SLL
:   Logical left shift.
    Shifts `rs1` by shift amount held in lower 5 bits of `rs2`.

    [R type]; $\funct3{001_2}$; $\funct7{0000000_2}$

SRL
:   Logical right shift.
    Shifts `rs1` by shift amount held in lower 5 bits of `rs2`.

    [R type]; $\funct3{101_2}$; $\funct7{0000000_2}$

SRA
:   Arithmetic right shift.
    Shifts `rs1` by shift amount held in lower 5 bits of `rs2`.

    [R type]; $\funct3{101_2}$; $\funct7{0100000_2}$

## Control instructions; J type

JAL
:   Jump and link.
    Adds offset to address of instruction to obtain jump target address.
    Stores address of instruction after jump instruction in `rd`.

    *   J assmbler pseudo-instruction is implemented in terms of JAL.
        I.e., `j n` is actually `jal x0, n`.

    [J type]; $\opcode{1101111_2}$

JALR
:   Indirect jump, or jump and link register.
    Adds offset to `rs1` to obtain jump target address,
    sets least significant bit to zero.
    Stores address of instruction after jump instruction in `rd`.

    [I type]; $\opcode{1100111_2}$; $\funct3{000_2}$

## Control instructions; B type

BEQ
:   Branch on equality.

    [B type]; $\funct3{000_2}$

BNE
:   Branch on inequality.

    [B type]; $\funct3{001_2}$

BLT
:   Branch on `rs1 < rs2`.

    *   BGT synthesized by reversing operands.

    [B type]; $\funct3{100_2}$

BLTU
:   Branch on `rs1 < rs2`; unsigned.

    *   BGTU synthesized by reversing operands.

    [B type]; $\funct3{110_2}$

BGE
:   Branch on `rs1 > rs2`.

    *   BLE synthesized by reversing operands.

    [B type]; $\funct3{101_2}$

BGEU
:   Branch on `rs1 > rs2`; unsigned.

    *   BLEU synthesized by reversing operands.

    [B type]; $\funct3{111_2}$

## Memory access instructions

The execution environment interface defines whether memory is little-endian or big-endian.
Endianness is byte-address invariant; if a byte is stored to some address in some endianness,
a byte-sized load from that address in any endianness restores that value.

Naturally aligned loads and stores are guaranteed to be atomic.

### Load instructions

For [I type] load instructions, $\opcode{0000011_2}$.

LW
:   Loads 32-bit value from memory.

    [I type]; $\funct3{010_2}$

LH
:   Loads 16-bit value from memory; value is sign-extended to 32 bits.

    [I type]; $\funct3{001_2}$

LHU
:   Loads 16-bit value from memory; value is zero-extended to 32 bits.

    [I type]; $\funct3{101_2}$

LB
:   Same as LH, but loads 8-bit value.

    [I type]; $\funct3{000_2}$

LBU
:   Same as LHU, but loads 8-bit value.

    [I type]; $\funct3{100_2}$

### Store instructions

For [S type] store instructions, $\opcode{0100011_2}$.

SW
:   Stores 32-bit value to memory.

    [S type]; $\funct3{010_2}$

SH
:   Stores 16-bit value to memory.

    [S type]; $\funct3{001_2}$

SB
:   Stores 8-bit value to memory.

    [S type]; $\funct3{000_2}$

## Memory ordering instructions

FENCE
:   Orders device I/O and memory access as viewed
    by other harts, external devices, or coprocessors.
    Immediate value encodes various aspects of the ordering.
    Register fields are currently unused and reserved for future use.

    [I type]; $\opcode{0001111_2}$

## Environment call and breakpoints

These are SYSTEM instructions.
For [I type] SYSTEM instructions, $\opcode{1110011_2}$.
The immediate value encodes the type of the instruction.

ECALL
:   System call.

    [I type]; $\funct7{000000000000}$; other bits 0

EBREAK
:   Break to debugger.

    [I type]; $\funct7{000000000001}$; other bits 0

## Hint instructions

HINT instructions do not affect visible architectural state.
They are encoded as integer instructions with `x0` as the destination register.

No HINT instructions are currently defined.
Specific encodings are reserved for standard and custom hints for use in the future.

## See also

*   [Registers](../registers)
*   [Instruction formats](../formats)
*   [RISC-V Notes](../)

[B type]: ../formats#b
[I type]: ../formats#i
[J type]: ../formats#j
[R type]: ../formats#r
[S type]: ../formats#s
[U type]: ../formats#u

## References
