open Common
module Log = (val Logger.create "main" : Logs.LOG)

let () = Log.info (fun m -> m "Starting up.")

let start_time = Unix.gettimeofday ()
let see_you () =
  Log.info (fun m -> m "Total time: %.2fs." (Unix.gettimeofday () -. start_time));
  Log.info (fun m -> m "The end. See you! :-)");
  exit 0

let problems =
  Log.debug (fun m -> m "Parsing problems.");
  let problems =
    !Config.problems_files
    |> List.map (Problem.from_file)
  in
  Log.debug (fun m -> m "Parsed %d problems succesfully" (List.length problems));
  problems

let () =
  if !Config.analyse then
    (
      Log.debug (fun m -> m "Analyse mode");
      problems |> List.iter (fun problem ->
          Log.info (fun m -> m "Begining the analysis of problem `%s`" (Problem.name problem));
          Problem.analyse problem);
      see_you ()
    )

let () =
  Log.debug (fun m -> m "Starting score logging routine");
  Lwt.async (fun () -> Engine.log_total_score_every ~time:5. problems)

let () =
  let once = !Config.stop in
  Log.debug (fun m -> m "Starting solvers.");
  Lwt_main.run (Engine.run_all_solvers_on_problems ~once problems);
  Engine.log_total_score problems;
  see_you ()
