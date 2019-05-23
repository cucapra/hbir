Lotus
=====

Lotus is a compiler for HBIR. Its name is totally a [*Blades of Glory*][bog] reference.


Setting Up
----------

The compiler is written in [OCaml][].
You will need it and the OCaml package management tool, [opam][].
First, install the dependencies:

    $ opam install . --deps-only

You can build and install the compiler:

    $ dune build && dune install

Type `lotus` to check whether everything's working.

Or, for quick iteration cycles while working on the compiler, you can skip building and installing.
Just use:

    $ dune exec lotus

to build and run the compiler.

[bog]: https://www.imdb.com/title/tt0445934/
[opam]: https://opam.ocaml.org
[ocaml]: http://www.ocaml.org
