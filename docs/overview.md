Overview & Motivation
=====================

The ["new golden age"][ga] of splintering computer architectures presents a new set of challenges for compilers.
Where today's compilers target fixed, slowly evolving ISAs,
modern efficiency-oriented hardware changes rapidly to offer new domain-specific features and design parameter tuning with each release.
By targeting a static, implementation-oblivious abstraction, compilers leave potential efficiency on the table.
Instead, compilers need a generalized way to customize code for a specific generation of hardware.

HBIR is a low-level hardware abstraction that differs from a traditional ISA by *exposing* hardware internals that ISAs are designed to hide.
Its philosophy is to embrace the fact that members of the target hardware family will differ for different applications over time.
To deal with hardware implementation details, a traditional ISA needs to either conceal them under a single virtual machine model or else bake a particular choice into the language semantics.
Instead, HBIR can express low-level hardware details *in the language itself*.
It offers a general way to describe the specific member of the hardware family that a program targets---or the range of hardware generations it supports.

By making these hardware target details explicit, HBIR enables:

- Portable compilers that can customize code for new hardware capabilities without modification.
- Portable compiled programs that encode their assumptions about the range of supported hardware from the past and into the future.
- An experimental platform for designing new hardware iterations and examining their impact on software performance.

The end goal is a more fluid relationship between hardware and software.
Intermediated by HBIR, architecture generations can evolve more quickly while software toolchains can more easily harness their new capabilities.

[ga]: https://cacm.acm.org/magazines/2019/2/234352-a-new-golden-age-for-computer-architecture/fulltext


The Target Machine
------------------

The current realization of HBIR targets the HammerBlade manycore processor, which is a grid of simple [RISC-V][] cores.
The manycore tiles have instruction caches but no data caches, and hence no coherence---instead, cores have a local data memory that acts as a scratchpad.
There is an on-chip network that lets cores read and write remote scratchpads.

The HBIR programming model primarily focuses on a SPMD style:
the program describes similar code to run on collections of tiles.
Each tile can use its index within a group to change the memory it computes on.
This programming model is well suited to regular, dense data-intensive applications.

Lotus, the HBIR compiler, emits C code to execute on the manycore and an accompanying host processor.

[risc-v]: https://riscv.org


Future Work
-----------

While HBIR currently focuses on static, SPMD, general-purpose manycore programming, there are a few big challenges that represent next steps for the abstraction:

- Incorporating accelerators.
  Aside from the homogeneous fabric of general-purpose cores, we imagine more customized iterations of HBIR will include fixed-function logic and communication structures.
  We want the HBIR machine model to incorporate arbitrary accelerators so software can transparently target them on targets where they are available.
- Dynamic reconfiguration.
  The configuration in HBIR is entirely static---it is fixed for the entire duration of a program.
  In the future, we want to use the same abstraction for configurations while letting them change at run time, in response to changing data characteristics and workload phases.
