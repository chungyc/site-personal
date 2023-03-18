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

JAL
:   Jump and link.  J type.

    Adds offset to address of instruction to obtain jump target address.
	Stores address of instruction after jump instruction in `rd`.

	J assmbler pseudo-instruction is implemented in terms of JAL.
	I.e., `j n` is actually `jal x0, n`.

JALR
:   Indirect jump, or jump and link register.  I type.

    Adds offset to `rs1` to obtain jump target address,
	sets least significant bit to zero.
	Stores address of instruction after jump instruction in `rd`.

BEQ
:   Branch on equality.  B type.

BNE
:   Branch on inequality.  B type.

BLT
:   Branch on `rs1 < rs2`.  B type.

    BGT synthesized by reversing operands.

BLTU
:   Branch on `rs1 < rs2`; unsigned.  B type.

    BGTU synthesized by reversing operands.

BGE
:   Branch on `rs1 > rs2`.  B type.

    BLE synthesized by reversing operands.

BGEU
:   Branch on `rs1 > rs2`; unsigned.  B type.

    BLEU synthesized by reversing operands.

## Load and store instructions

The execution environment interface defines whether memory is little-endian or big-endian.
Endianness is byte-address invariant; if a byte is stored to some address in some endianness,
a byte-sized load from that address in any endianness restores that value.

LW
:   Loads 32-bit value from memory.  I type.

LH
:   Loads 16-bit value from memory; value is sign-extended to 32 bits.  I type.

LHU
:   Loads 16-bit value from memory; value is zero-extended to 32 bits.  I type.

LB
:   Same as LH, but loads 8-bit value.  I type.

LBU
:   Same as LHU, but loads 8-bit value.  I type.

SW
:   Stores 32-bit value to memory.  S type.

SH
:   Stores 16-bit value to memory.  S type.

SB
:   Stores 8-bit value to memory.  S type.

Naturally aligned loads and stores are guaranteed to be atomic.

## Memory ordering instructions

FENCE
:   Orders device I/O and memory access as viewed
    by other harts, external devices, or coprocessors.
	I type.

	Immediate value encodes various aspects of the ordering.
	Register fields are currently unused and reserved for future use.

## Environment call and breakpoints

Technically there is a single SYSTEM instruction.
The immediate value encodes the type of the instruction.

ECALL
:   System call.  I type.  Unprivileged.

EBREAK
:   Break to debugger.  I type.  Unpriviledged.

## Hint instructions

HINT instructions do not affect visible architectural state.
They are encoded as integer instructions with `x0` as the destination register.

No HINT instructions are currently defined.
Specific encodings are reserved for standard and custom hints for use in the future.

## See also

*   [Registers](../registers)
*   [Instruction formats](../formats)

## References
