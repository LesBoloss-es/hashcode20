(* open ExtPervasives *)
module Log = (val Logger.create "common.problem" : Logs.LOG)

type t = { name : string }
[@@deriving show]

let name problem = problem.name

(* Parsing *)

let from_channel ~name (_ichan : in_channel) : t =
  { name }

let from_file (filename : string) : t =
  let name = Filename.basename filename in
  let ichan = open_in filename in
  let problem = from_channel ~name ichan in
  close_in ichan;
  problem

