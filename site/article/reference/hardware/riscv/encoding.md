---
title: RISC-V Instruction Length
description: Encoding specified in RISC-V for deriving the number of bits in an instruction.
published: 2023-03-09
include-math: true
include-bibliography-stylesheet: true
---

Excluding the compact instruction set, the standard instructions in RISC-V are all 32 bits.
Regardless, RISC-V leaves open the possibility of longer instructions.
This is encoded with specific bits in instructions [@riscv:user-level].

If $x$ is the binary encoding of an instruction in RISC-V, then if

* $x \land 11_2 \neq 11_2$, then it is 16 bits.

* $x \land 11_2 = 11_2$ and $x \land 11100_2 \neq 11100_2$, then it is 32 bits.

* $x \land 111111_2 = 011111_2$, then it is 48 bits.

* $x \land 1111111_2 = 0111111_2$, then it is 64 bits.

* $x \land 1111111_2 = 1111111_2$, $x \land (111_2 \times 2^{12}) = n$,
  and $n \neq 111_2$, then it is $(80+16n)$ bits.

* $x \land 1111111_2 = 1111111_2$ and $x \land (111_2 \times 2^{12}) = 111_2 \times 2^{12}$,
  then it will be 192 bits or longer.
  
  The specifics of encoding the length of instructions that are longer than 192 bits
  have yet to be specified.

## See also

*   [RISC-V Notes](./)

## References
