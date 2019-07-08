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

There is currently a lot of ongoing work with the compiler as well as
finalizing the semantics of the IR. Each section describes work that
needs to be done for both HBIR and Lotus.

### Long-Term Goals

The end-goal of the compiler tool-chain for HammerBlade consists of an
end-to-end compilation flow from high-level frameworks (TensorFlow,
PyTorch, Grappa, etc) to the underlying hardware. In addition, the
compiler tool-chain should also have profiling and dynamic
reconfiguration built in to allow dynamic reconfiguration of the
hardware based off changing application properties.

#### Frontend Integration with High-Level Frameworks

Frontend integration with existing compiler stacks for machine learning
and graph applications (TVM and a corresponding Graph compiler stack,
possibly GraphIt) is a more defined next step that creates an end-to-end
compilation flow. The compiler stack for graph applications can possibly
leverage GraphIt which is a DSL for graph applications that also has a
corresponding compiler which generates C++ code. The compiler stack for
machine learning will leverage TVM which is developed mainly by UW as a
compiler stack for various machine learning frameworks (essentially all
of them). TVM has a unified IR for machine learning and also supports
many different backends (including a [C backend](https://github.com/dmlc/tvm/pull/2161)).

Currently, HBIR exists as a human-readable and human-writable IR/DSL
that is used to easily program HammerBlade hardware instances (which
mainly run RISC-V assembly). The code segment currently uses C-like code
to represent high-level algorithmic details. While both TVM and GraphIt
can generate code that can be massaged easily to fit the code segment's
specifications; this defeats the purpose of having HBIR being
human-readable and human-writable. For future iterations, it is probably
better to move the code segment to look more like an IR that
encapsulates both TVM's and GraphIt's while also optimizing to allow
programmers to still write HBIR programs if they'd like. This is also
beneficial in that it removes a stage in our compiler toolchain (not
having to run either TVM and GraphIt and then Lotus).

#### Dynamic Reconfiguration

This is currently an unknown in terms of implementation and scope but
the end goal is to allow dynamic reconfiguration of the hardware based
off changing application properties. In other words, the compiler
tool-chain should be able to infer properties within an application,
whether this is done from the source code or any of the IRs, and
generate config, data, and possibly code segments of an HBIR program
(target segment stays the same for each target). The project proposal
mentions using machine learning to handle profiling and ultimately the
aforementioned inference.


### Type Checking in Lotus

Currently, there is no type checking done at compile-time for a HBIR
program compiled using Lotus. For the foreseeable future, HBIR will
remain as a human-readable and writable DSL to enable programming
HammerBlade which means type-checking an HBIR program at compile-time is
extremely important. Basic type-checking features to be added include

-   Ensure unique variable names are assigned to different HBIR data
    types.

-   Ensure access width is less than size for memory instances in
    target.

-   Ensure group indexes don't overlap in config.

-   Ensure group indexes don't exceed target dimensions in config.

-   Ensure flags don't conflict in data segment.

-   Ensure logical data structures don't exceed physical memory in
    target.

-   Basic code segment type-checking (although we can leverage gcc for
    this).

### Interpreter/Emulator

In addition to type checking, while HBIR is more human-writable than
writing a C-program using the low-level bsg primitives, it is still
difficult to get correct and simply running on any of the backends
doesn't provide a good debugging experience. Building a simple
interpreter or emulator to allow high-level verification of HBIR
programs is also important while HBIR is primarily used as a DSL.

### Compiling More Feature-Rich Code Segments

Programs utilizing vectors and simple memory access patterns are
currently functional when compiling using Lotus. More complicated
access-patterns such as memory replication, striping, or chunking is yet
to be tested. Applications using arrays are currently untested as well.

In addition to simply adding support for more applications, several
improvements to the code segment can make the IR more human-writable. An
example is allowing iterators to be used in for-loops. This abstracts
away the need for programmers to directly write how indexing should
change based off changing configurations.
