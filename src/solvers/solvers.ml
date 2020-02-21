open Common.Solver

(* Register here all the solvers with their names. *)

(* It is possible to repeat solvers, that is run them again on the same problem.
   A solver can thus be declared "repeatable" (option defaulting to false), and
   the engine, in non-stop mode, will call the solver again and again. This is
   particularly useful in case of solvers using randomness, for instance. *)

(* For solvers not using randomness, it is possible to define a "configurator"
   (option defaulting to [fun () -> Random.self_init ()]) that will be ran
   before running the solver. This allows for instance to have a solver
   switching between several configurations at every new call. It is important
   that this is done in a different function, because the solvers will be ran in
   forks, and thus nothing in the call of the solver will be persistent. *)

(* Defining a configurator sets repeatable to true. *)

let all = [
  make ~name:"greedy" Greedy.solve;
  make ~name:"niols" Niols.solve;
]
