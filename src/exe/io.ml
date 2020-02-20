open Common
module Log = (val Logger.create "io" : Logs.LOG)

module Solution = struct
  let file ~name ext =
    Filename.concat !Config.solutions_dir (name ^ ext)

  let lock ~name =
    try
      let ochan = open_out_gen [Open_creat; Open_excl] 0o644 (file ~name ".lock") in
      close_out ochan; true
    with
      Sys_error _ -> false

  let unlock ~name =
    Sys.remove (file ~name ".lock")

  let rec with_lock ~name f =
    if lock ~name then
      (let x = f () in unlock ~name; x)
    else
      with_lock ~name f

  let read_score ~name =
    let score_file = file ~name ".score" in
    try
      let ichan = open_in score_file in
      let score = int_of_string (input_line ichan) in
      close_in ichan;
      score
    with
    | Sys_error _ ->
      0
    | End_of_file ->
      Log.warn (fun m -> m "Got end of file while reading %s" score_file);
      0
    | Failure _ ->
      Log.warn (fun m -> m "%s doesn't contain an integer" score_file);
      0

  let write_score ~name score =
    let ochan = open_out (file ~name ".score") in
    output_string ochan ((string_of_int score) ^ "\n");
    close_out ochan

  let write_solver_name ~name solver =
    let ochan = open_out (file ~name ".solver") in
    output_string ochan (Solver.name solver ^ "\n");
    close_out ochan

  let add_solver_name ~name solver =
    let ochan = open_out_gen [Open_append] 0o644 (file ~name ".solver") in
    output_string ochan (Solver.name solver ^ "\n");
    close_out ochan

  let write_solution ~name solution =
    Solution.to_file (file ~name "") solution

  let write_if_better ~problem ~solver solution =
    let name = Problem.name problem in
    with_lock ~name @@ fun () ->
    let score = Solution.score problem solution in
    let score' = read_score ~name in
    if score > score' then
      (
        write_score ~name score;
        write_solver_name ~name solver;
        write_solution ~name solution;
      )
    else if score = score' then
      add_solver_name ~name solver
end
