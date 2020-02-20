(* open ExtPervasives *)
module Log = (val Logger.create "common.problem" : Logs.LOG)

type book = int
[@@deriving show]

type library =
  { content : book array ;
    signup_time : int ;
    books_per_day : int }
[@@deriving show]

type t =
  { name : string ;
    books : book array ;
    libraries : library array ;
    days : int }
[@@deriving show]

let name problem = problem.name

(* Analysing *)

let analyse problem =
  (* FIXME *)
  Log.info (fun m -> m "This is the analysis of problem `%s`." (name problem))

(* Parsing *)

let from_channel ~name (ichan : in_channel) : t =
  (* FIXME *)
  ignore ichan;
  { name }

let from_file (filename : string) : t =
  let name = Filename.basename filename in
  let ichan = open_in filename in
  let problem = from_channel ~name ichan in
  close_in ichan;
  problem

