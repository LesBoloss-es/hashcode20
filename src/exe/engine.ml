open Common
module Log = (val Logger.create "engine" : Logs.LOG)

type process_status = Unix.process_status =
  | WEXITED of int
  | WSIGNALED of int
  | WSTOPPED of int
[@@deriving show]

let pp_solver_problem_pid fmt (solver, problem, pid) =
  fpf fmt "%s/%s[%d]" (Solver.name solver) (Problem.name problem) pid

let run_solver_on_problem solver problem =
  Solver.persistent_configure solver;
  match Lwt_unix.fork () with
  | 0 ->
    (
      try
        Solver.configure solver;
        (* No need to copy problem, even if it is mutable, because we are in a fork. *)
        let solution = Solver.solve solver problem in
        Log.debug (fun m -> m "Got solution from solver; writing if better");
        Io.Solution.write_if_better ~problem ~solver solution;
        exit 0
      with
        exn ->
        Log.err (fun m ->
            m "Uncaught exception in %a:@\n%s@\n%s"
              pp_solver_problem_pid (solver, problem, 0)
              (Printexc.to_string exn)
              (Printexc.get_backtrace ()));
        exit 2
    )

  | pid ->
    Log.debug (fun m -> m "%a started" pp_solver_problem_pid (solver, problem, pid));
    Lwt_unix.waitpid [] pid >>= fun (_, status) ->
    if status = WEXITED 0 then
      Log.debug (fun m -> m "%a stopped with success" pp_solver_problem_pid (solver, problem, pid))
    else
      Log.warn (fun m -> m "%a stopped with status: %a"
                   pp_solver_problem_pid (solver, problem, pid) pp_process_status status);
    Lwt.return ()

let solver_problems_stream solver problems =
  problems
  |> Lwt_stream.of_list
  |> Lwt_stream.map (fun problem -> (solver, problem))

let solvers_problems_stream ?(only_repeatable=false) problems =
  Solvers.all
  |> (if only_repeatable then List.filter Solver.is_repeatable else Fun.id)
  |> Lwt_stream.of_list
  |> Lwt_stream.map_list (fun solver ->
      problems
      |> List.map (fun problem -> (solver, problem)))

let infinite_solvers_problems_stream problems =
  Lwt_stream.(
    append
      (solvers_problems_stream ~only_repeatable:false problems)
      (concat
         (from_direct
            (fun () -> Some (solvers_problems_stream ~only_repeatable:true problems))))
  )

let run_all_solvers_on_problems ~once problems =
  (if once then
     solvers_problems_stream problems
   else
     infinite_solvers_problems_stream problems)
  |>
  Lwt_stream.iter_n
    ~max_concurrency:!Config.nb_workers
    (fun (solver, problem) ->
       run_solver_on_problem solver problem)

let log_total_score problems =
  let open Io.Solution in
  let total_score =
    problems
    |> List.map
      (fun problem ->
         let name = Problem.name problem in
         with_lock ~name @@ fun () ->
         read_score ~name)
    |> List.fold_left (+) 0
  in
  Log.info (fun m -> m "Total score: %d" total_score)

let rec log_total_score_every ~time problems =
  Lwt_unix.sleep time >>= fun () ->
  log_total_score problems;
  log_total_score_every ~time problems
