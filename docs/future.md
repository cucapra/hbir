Future work
===========

There is currently a lot of ongoing work with the compiler as well as
finalizing the semantics of the IR. Each section describes work that
needs to be done for both HBIR and Lotus.

Long-term goals
---------------

The end-goal of the compiler tool-chain for HammerBlade consists of an
end-to-end compilation flow from high-level frameworks (TensorFlow,
PyTorch, Grappa, etc) to the underlying hardware. In addition, the
compiler tool-chain should also have profiling and dynamic
reconfiguration built in to allow dynamic reconfiguration of the
hardware based off changing application properties.

### Frontend integration with high-level frameworks

Frontend integration with existing compiler stacks for machine learning
and graph applications (TVM and a corresponding Graph compiler stack,
possibly GraphIt) is a more defined next step that creates an end-to-end
compilation flow. The compiler stack for graph applications can possibly
leverage GraphIt which is a DSL for graph applications that also has a
corresponding compiler which generates C++ code. The compiler stack for
machine learning will leverage TVM which is developed mainly by UW as a
compiler stack for various machine learning frameworks (essentially all
of them). TVM has a unified IR for machine learning and also supports
many different backends (including a C backend as of
https://github.com/dmlc/tvm/pull/2161).\
Currently, HBIR exists as a human-readable and human-writable IR/DSL
that is used to easily program HammerBlade hardware instances (which
mainly run RISC-V assembly). The code segment currently uses C-like code
to represent high-level algorithmic details. While both TVM and GraphIt
can generate code that can be massaged easily to fit the code segment's
specifications; this defeats the purpose of having HBIR being
human-readable and human-writable. For future iterations, it is probably
better to move the code segment to look more like an IR that
encapsualtes both TVM's and GraphIt's while also optimizing to allow
programmers to still write HBIR programs if they'd like. This is also
beneficial in that it removes a stage in our compiler toolchain (not
having to run either TVM and GraphIt and then Lotus).

### Dynamic Reconfiguration

This is currently an unknown in terms of implementation and scope but
the end goal is to allow dynamic reconfiguration of the hardware based
off changing application properties. In other words, the compiler
tool-chain should be able to infer properties within an application,
whether this is done from the source code or any of the IRs, and
generate config, data, and possibly code segments of an HBIR program
(target segment stays the same for each target). The project proposal
mentions using machine learning to handle profiling and ultimately the
aforementioned inference.

Type checking in Lotus
----------------------

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

Interpreter/Emulator
--------------------

In addition to type checking, while HBIR is more human-writable than
writing a C-program using the low-level bsg primitives, it is still
dificult to get correct and simply running on any of the backends
doesn't provide a good debugging experience. Building a simple
interpreter or emulator to allow high-level verification of HBIR
programs is also important while HBIR is primarily used as a DSL.

Compiling more feature-rich code segments
-----------------------------------------

Programs utilizing vectors and simple memory access patterns are
currently functional when compiling using Lotus. More complicated
access-patterns such as memory replication, striping, or chunking is yet
to be tested. Applications using arrays are currently untested as well.\
In addition to simply adding support for more applications, several
improvements to the code segment can make the IR more human-writable. An
example is allowing iterators to be used in for-loops. This abstracts
away the need for programmers to directly write how indexing should
change based off changing configurations.\
