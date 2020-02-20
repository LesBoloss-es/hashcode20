hashcode20
============

Preparing for the competition
-----------------------------

Clone this project:

    git clone git@github.com:Niols/hashcode20.git
    cd hashcode20

Prepare OPAM:

    opam switch import --switch hashcode20 opamswitch
    eval $(opam env)

Check that the project compiles:

    make

Usage
------

The tool run on one or several problems and writes solutions in the `solutions/`
directory, if they have a better score than what has been previously writen
there.

To test on one problem in particular, one can use simply:

    bin/hashcode20 -v /path/to/problem

This will run all the registered solvers once on the given problem, logging
everything that is done (`-v`, or `--verbose` logs the debug information). It is
also possible to specify several problems.

To run on all the problems in non-stopping mode:

    bin/hashcode20 --non-stop

This will run all the registered solvers on all the problems in `problems/` in a
loop, respecting priorities, and never stopping. This is particularly useful if
there are solvers depending on a random seed.

There are command line switches to redefine the problems or solutions directory.

Structure of the repository
---------------------------

- The directory `problems` should contain the given problem files.
- The directory `solutions` will be created automatically. It is ignored by the
  versioning system. The tool will write its suggestions there.
- The directory `src` contains the code of the tool including some machinery
  and, later, the definitions of problems, solutions and solvers.

Writing the basics
------------------

The very first thing to do is to write OCaml types for problem and solutions.
These can be found in `Problem.t` and `Solution.t`. One can then parallelise
between writing solvers (cf. next section) and writing a parser for problems
(`Problem.from_file`), a printer for solutions (`Solution.to_file`) and a
scoring function for solutions (`Solution.score`).

Writing a solver
----------------

A solver is a function of type `Problem.t -> Solution.t`. These functions can
(and should) be written in their own modules, in the `solvers/` directory. They
must then be declared in the list `Solvers.all`, in file `solvers/solvers.ml`.

A solver can be repeatable, eg. if it uses randomness to produce a different
result on the same problem, this goes by adding the flag `repeatable` in
`Solvers.all`.

A solver may also want to be configurable. Since all solvers are run in a fork,
there are two kinds of configurations: the one in the fork, and a persistent one
happening before the fork. The former is not really useful. The second one is
meant in case of a solver that can have several non-random configurations.
