The Lotus Compiler
==================

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
