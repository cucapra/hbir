The HBIR Language
================

Segments
--------

The following subsections describe the four segments of an HBIR program.
A valid HBIR program must have all four segments with strict
dependencies in terms of the following precedence: target, config, data,
code. The first three segments describe hardware configuration and
memory mappings while the last segment describes high-level algorithmic
details; hence the first three segments have very strict syntax and
little support for extraneous operations.

### Target

The **target** segment expresses the static parameters of the
HammerBlade hardware that the programmer would like to target. This
corresponds to a specific instance of the HammerBlade architecture which
can support varying amounts of tiles and memory.

    target {
        memory g[4] {
          size 8G;
          width 8B;
        };
        
        ...
    }

The first part of the segment declares the shared, global memories that
are available to the rest of the hardware. Programmers specify a name,
number of memories, size, and access width for the instance of memory.
The instance of memory can then be referenced in different segments of
an HBIR program. In the code snippet above, we are declaring a 4 global
memories named `g` with a size of 8GB and an access width of 8B. Other
segments of an HBIR program can reference `g` by referencing `target.g` and
can also reference any of the fields of `g` as well. Referencing memory
with discrete, real sizes are often not useful so memory is typically
mapped to logical data structures for use in the code segment (described
later).

    target {
        ...
        
        tile t[4][4] {
            memory l [4] {
              size 16K;
              width 8B;
            };
        };
    }

The next part of the segment declares the statically defined fabric in
the target architecture. Programmers specify a name and the dimensions
of the tile array. Compute resources (CGRA and CPU) are implicitly
included in a tile declaration but programmer can optionally specify
local memory in the same manner global memories are declared. In the
code snippet above, a tile array of 4x4 is declared, with each tile
having a 16KB local memory with an access width of 8B. Similar to global
memory, programmers can reference this memory by referencing the tile's
name. For example, `target.t.l.size` accesses the size of the local memory
in the tile array. Notice that both memory accesses and tile accesses
don't allow indexing to specific resources. This is because we don't
expect the tiles to be homogeneous as well as the global memory
structures available.

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

In the above code snippet, we are modeling the architecture seen in
Figure 1 by mapping the 2 L2 caches as a global memory with each having
8GB and 8B access width. It also has has 128x64 tiles with each one
having a small local memory of 64KB and also an access width of 8B.


### Config

The **config** segment expresses the dynamic configuration of the
hardware that best fits an application. This section overlays any
physical topology onto the fabric by grouping together resources defined
in the target segment. Currently, groups only reference physical tiles
and do not use any global memories or local memories defined in the
target segment.

    config {
        group tg[4][4] {
            tile target.t[x][y];
        };
    }

To declare a group, users specify a name, the dimensions of the group,
and specifies how the group's tiles index into the physical tiles
defined in the target segment. The most common group involves just
specifying a fixed-size, flat grid of tiles from the tile array.
Following with previous snippets of code, the tile array is named t in
our example and the programmer has specified a flat grid of 16 tiles
from the tile array. The variables x and y are used to represent
indexing into specific tiles in the array which is necessary for more
complicated topologies.

    config {
        group tg[target.t.x_max][target.t.y_max] {
            tile target.t[x][y];
        };
    }

Programmers can also use `x_max` and `y_max` in reference to the tile
array to represent the maximum dimensions of the declared array. This
prevents hard-coding parameters in this segment which translates to
statically allocating groups based off *specific* hardware rather than
having this segment work off of a possibly changing target segment.

    config {
        group tga[2][4] {
            tile target.t[x][y];
        };
        
        group tgb[2][4] {
            tile target.t[x+2][y];
        };
    }

Programmers are also allowed to declare multiple groups in the segment.
It is important that indexing into the tile array in different groups
inherently prevent any overlapping tiles and any compilers built for
HBIR should check this at compile-time. The code snippet above declares
two groups `tga` and `tgb` which are both rectangles with dimension 2x4 but
maps them next to each other.

    config {
        group grid[target.t.y_max] {
          group row[target.t.x_max] {
            tile target.t[grid.x][row.x];
          };
        };
    }

Finally, programmers can also nest groups.

### Data

The data segment expresses how logical data structures, currently
vectors and arrays, map to to physical memory, both global and local,
declared in the target segment. The data structures declared here are
used in the code segment to express the high-level
algorithm/application.

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

The programmer specifies a name, the type (`int`, `float`, `bool`) and
dimension of the data structure (i.e., vector or array of a certain size),
how it maps to the declared groups, and several flags. Currently, flags
for chunked, replicated, or striped allow programmers to specify how
data should be distributed for the application. A host or device flag
can also be set to specify whether the data is an input (host) or output
(device) of the algorithm. Simple constant declarations are also allowed
as often vectors and arrays use the same dimensions. These have to be
declared before declaring any memory mappings.

The code snippet above declares memory mappings to be used for a
vector-vector add application (i.e., `C = A+B`). In this example, A and B
are declared as integer vectors with a size of 500 that map to the
global memory g defined in the target segment. The host flag is also set
to indicate these two arrays are considered input arrays to the
application and populated by a host. `C` is declared as an integer vector
with a size of 500 that also maps to global memory `g` defined in the
target segment. The device flag is set to indicate that the data
structure is an output and will be populated by the application.

    data{
        ...
        C: int[dim] = block[target.t.x_max][target.t.y_max] {
            target.g[x][y];
            chunked;
            device;
        };
        ...
    }

Other flags are chunked, replicated, and striped which change how the
data is distributed across the data structure.

### Code

The code segment expresses the high-level application by tying it to
groups defined in the config segment and using logical data structures
defined in the data segment. This operates similar to CUDA where
thread/block/grids determine the different execution contexts as well as
the memory that's used. Programmers specify groups as well as indexing
and the code that should run on tiles in this execution context.
Programmers are also allowed to write simple code that lives outside of
groups (global).

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

The current implementation of the language exposes some `bsg_manycore`
specific functions and also doesn't infer much from the application at
all but the end-goal is to only have high-level, tile-independent,
C-like code with the only reference to hardware being the group that the
code is tied to. The above code snippet is how a simple vector-vector
add is currently implemented. While only one group is referenced above,
multiple groups can be referenced. In addition, within each group,
single tiles can be referenced to give specific code in addition to
generalized code that runs on all tiles in a group (by using symbolic
indexes). In this example, every tile inside the group `tg` runs a basic
for loop that has data indexing into the global arrays `A`, `B`, and `C`
hard-coded. Tile (0, 0) is also given a special instruction to call
`bsg_finish()`. Global code is also written to declare `g_done_flag`.


Data Types
----------

This section outlines different data types in an HBIR program (outside
of the code segment), where the data type is valid, and a brief
description on its use:

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


Code Segment SPMD Specification
-------------------------------

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
