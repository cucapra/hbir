Lotus
=====

Lotus is a compiler for HBIR. Its name is totally a [*Blades of Glory*][bog] reference.

We need to install two dependencies, [Dune][] and [Menhir][]:

    $ opam install . --deps-only

Build by typing:

    $ dune build bin/lotus.bc

Run an example:

    $ echo '8*(3+2)+2' | dune exec bin/lotus.bc

Install a Lotus executable by running:

    $ dune build && dune install

[dune]: https://github.com/ocaml/dune
[menhir]: http://gallium.inria.fr/~fpottier/menhir/
[lexyacc]: https://caml.inria.fr/pub/docs/manual-ocaml/lexyacc.html
[utop]: https://github.com/diml/utop
[bog]: https://www.imdb.com/title/tt0445934/
