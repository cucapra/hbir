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
