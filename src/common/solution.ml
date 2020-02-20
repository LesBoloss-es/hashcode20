open ExtPervasives
module Log = (val Logger.create "common.solution" : Logs.LOG)
open Problem

type t = (library * book list) list
[@@deriving show]

let to_channel ochan solution : unit =
  output_string ochan (soi (List.length solution) ^ "\n");
  solution |> List.iter (fun (library, books) ->
      output_string ochan (soi library.lid ^ " " ^ soi (List.length books) ^ "\n");
      books
      |> List.map (fun book -> book.bid)
      |> List.map soi
      |> String.concat " "
      |> output_string ochan;
      output_string ochan "\n")

let to_file (filename : string) (solution : t) : unit =
  let ochan = open_out filename in
  to_channel ochan solution;
  close_out ochan

let score problem solution =
  (* FIXME *)
  ignore problem;
  ignore solution;
  0
