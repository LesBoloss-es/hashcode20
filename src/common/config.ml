open ExtPervasives

let stop = ref true

let solutions_dir = ref "solutions"
let problems_dir = ref "problems"
let problems_files = ref []
let nb_workers = ref 8

let loglevel = ref Logs.Info

let set ref_ val_ () =
  ref_ := val_

let specs =
  let open Arg in
  align [
    "--solutions", Set_string solutions_dir, spf "DIR Sets the solutions directory to DIR (default: %s)" !solutions_dir ;
    "--problems",  Set_string problems_dir,  spf "DIR Sets the problems directory to DIR. This will only be used if no problems are given in the arguments (default: %s)" !problems_dir ;
    "--workers",   Set_int    nb_workers,    spf "NB Sets the number of workers to NB (default: %d)" !nb_workers ;

    "--non-stop",  Clear stop,               spf " Sets the tool in non-stop mode" ;

    "--quiet",     Unit (set loglevel Logs.Warning), spf " Sets quiet mode";
    "-q",          Unit (set loglevel Logs.Warning), spf " Short for --quiet";
    "--verbose",   Unit (set loglevel Logs.Debug),   spf " Sets verbose mode";
    "-v",          Unit (set loglevel Logs.Debug),   spf " Short for --verbose";
  ]

let handle_file problem =
  if not Sys.(file_exists problem) then
    raise (Arg.Bad (spf "Given file `%s` does not exist." problem));
  if Sys.is_directory problem then
    raise (Arg.Bad (spf "Given file `%s` is a directory." problem));
  problems_files := !problems_files @ [problem]

let usage = spf "Usage: %s [OPTIONS] [PROBLEM...]" Sys.argv.(0)

let () = Arg.parse specs handle_file usage

let () =
  if !problems_files = [] then
    (
      !problems_dir
      |> Sys.readdir
      |> Array.to_list
      |> List.sort String.compare
      |> List.map (Filename.concat !problems_dir)
      |> (:=) problems_files
    );
