(* open ExtPervasives *)
module Log = (val Logger.create "common.solution" : Logs.LOG)

type t = (Problem.library * Problem.book list) list
[@@deriving show]

let to_channel ochan solution : unit =
  (* FIXME *)
  ignore ochan;
  ignore solution

let to_file (filename : string) (solution : t) : unit =
  let ochan = open_out filename in
  to_channel ochan solution;
  close_out ochan

let score problem solution =
  (* FIXME *)
  ignore problem;
  ignore solution;
  0
