open Common
open Problem

module Log = (val Logger.create "solver.greedy" : Logs.LOG)

let simulation_step = 10

let steps_per_update = 20

type my_library = {
  library : Problem.library;
  mutable signed_up_at : int;
  mutable selection_score : int;
  mutable scanned : book list;
}

type my_problem = {
  problem : Problem.t;
  my_libs : my_library array;
  mutable signed_libraries : my_library list;
  mutable next_signup_in : int;
  mutable cur_day : int;
}

let library_score library rem_days =
  let contents = library.content in
  let max_books = (rem_days - library.signup_time) * library.books_per_day in
  let score = ref 0 in
  let books = ref 0 in
  for i = 0 to Array.length contents - 1 do
    if !books < max_books && not contents.(i).selected then begin
      score := contents.(i).score + !score;
      incr books;
    end
  done;
  !score

let convert_library library days =
  let score = library_score library days in
  {
    library;
    signed_up_at = -1;
    selection_score = score;
    scanned = [];
  }

let convert_problem problem =
  let libraries = problem.libraries in
  let my_libs = Array.map (fun l -> convert_library l problem.days) libraries in
  {my_libs; problem; signed_libraries = []; next_signup_in = 0; cur_day = 0}

let update_scores problem =
  let rem_days = problem.problem.days - problem.cur_day in
  for i = 0 to Array.length problem.my_libs - 1 do
    let lib = problem.my_libs.(i) in
    if lib.signed_up_at < 0 then
      lib.selection_score <- library_score lib.library rem_days
  done

let exclude_from library book =
  let content_indir = library.library.content_by_bid in
  try
    let idx = Hashtbl.find content_indir book.bid in
    ignore idx;
    library.selection_score <- library.selection_score - book.score
  with
    Not_found -> ()

let exclude_all_books_from_library signed_library library =
  let books = signed_library.library.content in
  for i = 0 to Array.length books - 1 do
    exclude_from library books.(i)
  done

let exclude_book_from_all_libraries problem book =
  let libs = problem.my_libs in
  for i = 0 to Array.length libs - 1 do
    exclude_from libs.(i) book
  done

let exclude_signup problem library =
  let libs = problem.my_libs in
  for i = 0 to Array.length libs - 1 do
    if libs.(i).signed_up_at < 0 then
      exclude_all_books_from_library library libs.(i)
  done

let best_library_to_signup problem =
  let rem_days = problem.problem.days - problem.cur_day in
  let libs = problem.my_libs in
  let max = ref 0 in
  let choice = ref None in
  for i = 0 to Array.length libs - 1 do
    if (libs.(i).selection_score >= !max || !choice = None) && libs.(i).signed_up_at < 0 
    && rem_days > libs.(i).library.signup_time then begin
      choice := Some libs.(i);
      max := libs.(i).selection_score
    end
  done;
  !choice

let pick_library problem =
  match best_library_to_signup problem with
  | None -> None
  | Some lib ->
    let books = lib.library.content in
    for i = 0 to Array.length books - 1 do
      exclude_book_from_all_libraries problem books.(i);
      books.(i).selected <- true
    done;
    Some lib

let pick_best_books library step =
  let contents = library.library.content in
  let bpd = library.library.books_per_day in
  let max_books = step * bpd in
  let books = ref [] in
  let n_books = ref 0 in
  for i = 0 to Array.length contents - 1 do
    let book = contents.(i) in
    if not book.scanned && !n_books < max_books then begin
      incr n_books;
      books := book :: !books
    end
  done;
  !books

let simulation_step problem =
  let rem_days = problem.problem.days - problem.cur_day in
  let step = min rem_days (min problem.next_signup_in simulation_step) in
  if step > 0 then begin
    Log.debug (fun fmt -> fmt "simulating step of length %i" step);
    List.iter (fun lib ->
      let signed_up = lib.signed_up_at <= problem.cur_day in
      if signed_up then begin
        let books = pick_best_books lib step in
        List.iter (fun book -> book.Problem.scanned <- true) books;
        lib.scanned <- books @ lib.scanned;
      end) problem.signed_libraries
  end;
  problem.cur_day <- problem.cur_day + step;
  problem.next_signup_in <- problem.next_signup_in - step;
  if problem.next_signup_in <= 0 then begin
    Log.debug (fun fmt -> fmt "looking for signup");
    match pick_library problem with
    | Some lib -> 
      Log.debug (fun fmt -> fmt "next library found");
      problem.next_signup_in <- lib.library.signup_time;
      lib.signed_up_at <- problem.cur_day + lib.library.signup_time;
      problem.signed_libraries <- lib :: problem.signed_libraries
    | None -> problem.next_signup_in <- 1000000
  end
    
let rec solve_problem problem =
  let rem_days = problem.problem.days - problem.cur_day in
  Log.debug (fun fmt -> fmt "day %i/%i" problem.cur_day problem.problem.days);
  if rem_days > 0 then begin
    simulation_step problem;
    solve_problem problem
  end

let build_solution problem =
  List.map (fun lib -> (lib.library, lib.scanned)) problem.signed_libraries

let solve problem =
  let problem = convert_problem problem in
  solve_problem problem;
  Log.debug (fun fmt -> fmt "solution found");
  build_solution problem
