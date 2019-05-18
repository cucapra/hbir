**Abstract.** HBIR is an intermediate representation / DSL for the
HammerBlade Project that expresses the static and dynamic configuration
of the target hardware as well as the high-level algorithm to be run.
HBIR programs specify the hardware parameters of the target and how an
algorithm should use the hardware.

Overview
========

Terminology {#sec:terms}
-----------

Throughout the remainder of this document, the following nomenclature
will be used for ease of explanation:

-   HammerBlade -- Project lead by Michael Taylor and Adrian Sampson for
    a new kind of continuously reconfigurable polymorphic hardware as
    well as a compiler toolchain.

-   HBIR -- HammerBlade Intermediate Representation to express both
    hardware and software details.

-   Lotus -- Prototype compiler for HBIR.

-   Chazz -- Utilities for running HammerBlade in F1

-   BSG Manycore -- Source code from the Bespoke Silicone Group at UW
    that implements the proposed tile based architecture.

-   BRG Manycore -- Forked copy of BSG Manycore that the Batten Research
    Group is working on to implement various accelerator architectures.

-   TVM -- Tensor Virtual Machine, a deep learning compiler stack.

-   GVM -- Corresponding graph compiler stack that is yet to be built.

Target Architecture Model
=========================

At a high level, our target architecture is a many-core fabric with each
tile having a small CGRA, small local memory, and a small CPU. Each tile
can communicate with each other via remote memory access and also has
access to a larger global memory. While still in its preliminary stages,
the architecture attempts to close the gap between completely
reconfigurable architectures (FPGA) and static accelerators (specialized
accelerator ASICs) by allowing course-grain reconfiguration to represent
three idealized architectures -- massive manycores, vector, and dataflow
-- and also a wide range of combinations between them.\
A programmer targeting this architecture will write high-level
applications using a wide-range of machine-learning and graph frameworks
(such as TensorFlow, PyTorch, and Grappa) and then use our compiler
tool-chain to target the architecture by configuring the tiles to a
well-fit topology as well as generating code that the hardware can run.
As the project is in it's early stages and lacks frontend support with
actual frameworks currently; several attempts have been made to further
programmability during the initial phase.\
The first approach to programming HammerBlade consists of writing C
programs and using a library of low-level functions to handle inter-tile
communication and configuration of the hardware. While this has been
integral to verifying the hardware design, it requires an intimate
knowledge of how inter-tile coordination should happen in the hardware
in order to map to an application. As a solution, an intermediate
representation that encapsulates both hardware configuration details and
algorithmic details has been proposed. This document contains a
specification of the intermediate language at its current state as well
as an overview of where it should go in the future.\

HBIR Segments
=============

The following subsections describe the four segments of an HBIR program.
A valid HBIR program must have all four segments with strict
dependencies in terms of the following precedence: target, config, data,
code. The first three segments describe hardware configuration and
memory mappings while the last segment describes high-level algorithmic
details; hence the first three segments have very strict syntax and
little support for extraneous operations.

Target
------

![HammerBlade architecture](hammerblade_arch.png){width="50%"}

[\[fig:my\_label\]]{#fig:my_label label="fig:my_label"}

The **target** segment expresses the static parameters of the
HammerBlade hardware that the programmer would like to target. This
corresponds to a specific instance of the HammerBlade architecture which
can support varying amounts of tiles and memory.\

``` {.c language="C" caption="Global memory instantiation"}
target {
    memory g[4] {
      size 8G;
      width 8B;
    };
    
    ...
}
```

The first part of the segment declares the shared, global memories that
are available to the rest of the hardware. Programmers specify a name,
number of memories, size, and access width for the instance of memory.
The instance of memory can then be referenced in different segments of
an HBIR program. In the code snippet above, we are declaring a 4 global
memories named g with a size of 8GB and an access width of 8B. Other
segments of an HBIR program can reference g by referencing target.g and
can also reference any of the fields of g as well. Referencing memory
with discrete, real sizes are often not useful so memory is typically
mapped to logical data structures for use in the code segment (described
later).\

``` {.c language="C" caption="Fabric instantiation without local memory"}
target {
    ...
    
    tile t[4][4] {};
}
```

``` {.c language="C" caption="Fabric instantiation with local memory"}
target {
    ...
    
    tile t[4][4] {
        memory l [4] {
          size 16K;
          width 8B;
        };
    };
}
```

The next part of the segment declares the statically defined fabric in
the target architecture. Programmers specify a name and the dimensions
of the tile array. Compute resources (CGRA and CPU) are implicitly
included in a tile declaration but programmer can optionally specify
local memory in the same manner global memories are declared. In the
code snippet above, a tile array of 4x4 is declared, with each tile
having a 16KB local memory with an access width of 8B. Similar to global
memory, programmers can reference this memory by referencing the tile's
name. For example, target.t.l.size accesses the size of the local memory
in the tile array. Notice that both memory accesses and tile accesses
don't allow indexing to specific resources. This is because we don't
expect the tiles to be homogeneous as well as the global memory
structures available.\

``` {.c language="C" caption="128x64 HammerBlade Instance Target Section"}
target {
  memory g[2] {
    size 8G;
    width 8B;
  };

  tile t[128][64] {
    memory l {
      size 64K;
      width 8B;
    };
  };
}
```

In the above code snippet, we are modeling the architecture seen in
Figure 1 by mapping the 2 L2 caches as a global memory with each having
8GB and 8B access width. It also has has 128x64 tiles with each one
having a small local memory of 64KB and also an access width of 8B.\

Config
------

The **config** segment expresses the dynamic configuration of the
hardware that best fits an application. This section overlays any
physical topology onto the fabric by grouping together resources defined
in the target segment. Currently, groups only reference physical tiles
and do not use any global memories or local memories defined in the
target segment.\

``` {.c language="C" caption="Group with static parameters"}
config {
    group tg[4][4] {
        tile target.t[x][y];
    };
}
```

To declare a group, users specify a name, the dimensions of the group,
and specifies how the group's tiles index into the physical tiles
defined in the target segment. The most common group involves just
specifying a fixed-size, flat grid of tiles from the tile array.
Following with previous snippets of code, the tile array is named t in
our example and the programmer has specified a flat grid of 16 tiles
from the tile array. The variables x and y are used to represent
indexing into specific tiles in the array which is necessary for more
complicated topologies.\

``` {.c language="C" caption="Group with symbolic parameters"}
config {
    group tg[target.t.x_max][target.t.y_max] {
        tile target.t[x][y];
    };
}
```

Programmers can also use x\_max and y\_max in reference to the tile
array to represent the maximum dimensions of the declared array. This
prevents hard-coding parameters in this segment which translates to
statically allocating groups based off **specific** hardware rather than
having this segment work off of a possibly changing target segment.\

``` {.c language="C" caption="Multiple groups"}
config {
    group tga[2][4] {
        tile target.t[x][y];
    };
    
    group tgb[2][4] {
        tile target.t[x+2][y];
    };
}
```

Programmers are also allowed to declare multiple groups in the segment.
It is important that indexing into the tile array in different groups
inherently prevent any overlapping tiles and any compilers built for
HBIR should check this at compile-time. The code snippet above declares
two groups tga and tgb which are both rectangles with dimension 2x4 but
maps them next to each other.\

``` {.c language="C" caption="Nested groups"}
config {
    group grid[target.t.y_max] {
      group row[target.t.x_max] {
        tile target.t[grid.x][row.x];
      };
    };
}
```

Finally, programmers can also nest groups.\

Data
----

The data segment expresses how logical data structures, currently
vectors and arrays, map to to physical memory, both global and local,
declared in the target segment. The data structures declared here are
used in the code segment to express the high-level
algorithm/application.\

``` {.c language="C" caption="Declaring vector mappings for memory"}
data{
    const dim = 500;

    A: int[dim] = block[target.t.x_max][target.t.y_max] {
        target.g[x];
        host;
    };

    B: int[dim] = block[target.t.x_max][target.t.y_max] {
        target.g[x];
        host;
    };

    C: int[dim] = block[target.t.x_max][target.t.y_max] {
        target.g[x];
        device;
    };
}
```

The programmer specifies a name, the type (int, float, boolean) and
dimension of the data structure (ie: vector or array of a certain size),
how it maps to the declared groups, and several flags. Currently, flags
for chunked, replicated, or striped allow programmers to specify how
data should be distributed for the application. A host or device flag
can also be set to specify whether the data is an input (host) or output
(device) of the algorithm. Simple constant declarations are also allowed
as often vectors and arrays use the same dimensions. These have to be
declared before declaring any memory mappings.\
The code snippet above declares memory mappings to be used for a
vector-vector add application (ie: C = A+B). In this example, A and B
are declared as integer vectors with a size of 500 that map to the
global memory g defined in the target segment. The host flag is also set
to indicate these two arrays are considered input arrays to the
application and populated by a host. C is declared as an integer vector
with a size of 500 that also maps to global memory g defined in the
target segment. The device flag is set to indicate that the data
structure is an output and will be populated by the application.\

``` {.c language="C" caption="Declaring vector mappings for memory"}
data{
    ...
    C: int[dim] = block[target.t.x_max][target.t.y_max] {
        target.g[x][y];
        chunked;
        device;
    };
    ...
}
```

Other flags are chunked, replicated, and striped which change how the
data is distributed across the data structure.\

Code
----

The code segment expresses the high-level application by tying it to
groups defined in the config segment and using logical data structures
defined in the data segment. This operates similar to CUDA where
thread/block/grids determine the different execution contexts as well as
the memory that's used. Programmers specify groups as well as indexing
and the code that should run on tiles in this execution context.
Programmers are also allowed to write simple code that lives outside of
groups (global).\

``` {.c language="C" caption="Declaring vector mappings for memory"}
code{
    int g_done_flag = 0;

    config.tg[0][0]{
        bsg_finish();
    }

    config.tg[x][y]{
        for(int i = x+(y*x_max); i<csize; i=i+1){
            C[i] = A[i] + B[i];
        }
        g_done_flag = 1;
    }
}
```

The current implementation of the language exposes some bsg\_manycore
specific functions and also doesn't infer much from the application at
all but the end-goal is to only have high-level, tile-independent,
C-like code with the only reference to hardware being the group that the
code is tied to. The above code snippet is how a simple vector-vector
add is currently implemented. While only one group is referenced above,
multiple groups can be referenced. In addition, within each group,
single tiles can be referenced to give specific code in addition to
generalized code that runs on all tiles in a group (by using symbolic
indexes). In this example, every tile inside the group tg runs a basic
for loop that has data indexing into the global arrays A, B, and C
hard-coded. Tile (0, 0) is also given a special instruction to call
bsg\_finish(). Global code is also written to declare g\_done\_flag.

HBIR Data types
===============

This section outlines different data types in an HBIR program (outside
of the code segment), where the data type is valid, and a brief
description on its use.

  Name         Valid Segments   Definition
  ------------ ---------------- -------------------------------------------------------------------
  memory       target           Used to declare a memory instance.
  size         target           Field in memory instance.
  width        target           Field in memory instance.
  tile         target           Used to declare a tile.
  group        config           Used to declare a group.
  block        config, data     Used to declare and refer to an execution context within a group.
  chunked      data             Used to specify a chunked data orientation.
  replicated   data             Used to specify a replicated data orientation.
  striped      data             Used to specify a striped data orientation.
  host         data             Used to specify that a logical data is an input.
  device       data             Used to specify that a logical data is an output.

Code Segment SPMD specification
===============================

The code segment uses a simple, C-like, SSA-form imperative language.
This section outlines basic features that the language currently
supports.

-   Generic types -- Supports ints, floats, and booleans as generics.

-   Basic arithmetic expressions -- Supports basic arithmetic and
    boolean operations with precedence rules being equivalent to C.

-   Declaration statements -- Supports static declarations of arrays and
    basic generic types.

-   Control flow statements -- Supports basic if-else, while loops, for
    loops, and break statements.

-   Miscellaneous features -- Supports printing as a primitive.

Lotus
=====

Lotus is the first reference compiler for HBIR that is currently under
development. The current implementation allows very basic HBIR programs
to be compiled and built on a RTL Simulator (BSG Manycore) and also a
hardware instance running on AWS F1. All of the HBIR features specified
in the above section are implemented here and this document will be
updated as the compiler is developed further.

Building and Running
--------------------

The compiler is implemented in OCaml and uses ocamllex for the Lexer and
Menhir for the Parser.\

``` {.c language="C" caption="Build Lotus"}
dune build bin/lotus.bc
dune install 
```

After cloning the HBIR repository, build the compiler by running the
build command in the lotus directory. Afterwards, run the install
command to install an image to use when executing the compiler.\

``` {.c language="C" caption="Declaring vector mappings for memory"}
lotus -bsg examples/vvadd.hbir 
```

After installing a Lotus image, simply run the compiler and pass in a
flag to indicate the target as well as the HBIR program to compile.
Depending on the hardware target and the parameters passed, the compiler
will either generate output files, or print to stdout. Currently, users
can pass in the following flags:

-   pp -- Compiles the given file with the pretty printer.

-   gcc -- Generates code that can be compiled by gcc.

-   bsg -- Generates code and a Makefile that can be run on the BSG
    Manycore RTL Simulator.

-   f1 -- Generates device and host code that can be run on the hardware
    instance on AWS F1.

-   v -- Prints contents in emitted files as well as any verbose logging
    information.

Backends
--------

The compiler has two main backends that are supported. The RTL Simulator
backend lives in manycore.ml and the F1 instance lives in f1.ml.
Currently, both implementations are identical in terms of the HBIR
features that is supported. The main difference between the two backends
is in the intermediate files that are generated which are then built on
the corresponding target. For the RTL Simulator, the compiler generates
a C program, main.c, and a Makefile. Moving both of these files to a
directory within the bsg\_manycore respository and then running make
will run the compiled program on the simulator. For the F1 instance,
device.c, host.c, and a Makefile is generated. Users are then instructed
to use Chazz to start the F1 instance before running the programs. More
changes between the backends are expected as the compiler is developed
further (documented below).

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

Appendix
========

Reference links {#ssec:resources}
---------------

-   HBIR GitHub repository -- https://github.com/cucapra/hbir

-   Lotus GitHub repository --
    https://github.com/cucapra/hbir/tree/master/lotus

-   Chazz GitHub repository -- https://github.com/cucapra/chazz
