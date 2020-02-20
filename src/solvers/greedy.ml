open Common
open Problem

module Log = (val Logger.create "solver.greedy" : Logs.LOG)

type my_library = {
  library : Problem.library;
  mutable signed_up_since : int;
  mutable selection_score : int;
  mutable score : int
}

type my_problem = {
  problem : Problem.t;
  my_libs : my_library array
}

let library_score library =
  let contents = library.content in
  let score = ref 0 in
  for i = 0 to Array.length contents - 1 do
    score := contents.(i).score + !score;
  done;
  !score

let convert_library library =
  let score = library_score library in
  {
    library;
    signed_up_since = -1;
    selection_score = score;
    score
  }

let convert_problem problem =
  let libraries = problem.libraries in
  let my_libs = Array.map convert_library libraries in
  {my_libs; problem}

let exclude_from ?(scanned=false) library book =
  let content = library.library.content in
  let content_indir = library.library.content_by_bid in
  try
    let idx = Hashtbl.find content_indir book.bid in
    if scanned then begin
      content.(idx) <- {book with score = 0};
      library.score <- library.score - book.score;
    end;
    library.selection_score <- library.selection_score - book.score
  with
    Not_found -> ()

let exclude_all_books_from_library ?(scanned=false) signed_library library =
  let books = signed_library.library.content in
  for i = 0 to Array.length books - 1 do
    exclude_from ~scanned library books.(i)
  done

let exclude_book_from_all_libraries ?(scanned=false) problem book =
  let libs = problem.my_libs in
  for i = 0 to Array.length libs - 1 do
    exclude_from ~scanned libs.(i) book
  done

let exclude_signup problem library =
  let libs = problem.my_libs in
  for i = 0 to Array.length libs - 1 do
    if libs.(i).signed_up_since < 0 then
      exclude_all_books_from_library library libs.(i)
  done

let best_library_to_signup problem =
  let libs = problem.my_libs in
  let max = ref 0 in
  let choice = ref None in
  for i = 0 to Array.length libs - 1 do
    if (libs.(i).selection_score >= !max || !choice = None) && libs.(i).signed_up_since < 0 then begin
      choice := Some libs.(i);
      max := libs.(i).selection_score
    end
  done;
  !choice

let pick_best_books library days =
  let contents = library.library.content in
  let bpd = library.library.books_per_day in
  let signup = library.library.signup_time in
  let rem_days = days - signup in
  let max_books = rem_days * bpd in
  let books = ref [] in
  for i = 0 to min (Array.length contents - 1) (max_books - 1) do
    let book = contents.(i) in
    if book.score > 0 then
      books := book :: !books
  done;
  !books

let solve_library problem library days =
  let books = pick_best_books library days in
  List.iter (fun book -> exclude_book_from_all_libraries ~scanned:true problem book) books;
  (library.library, books)

let rec solve_problem problem day =
  Log.debug (fun fmt -> fmt "day %i/%i" day problem.problem.days);
  match best_library_to_signup problem with
  | None -> []
  | Some lib -> begin
    Log.debug (fun fmt -> fmt "library found");
    let signup = lib.library.signup_time in
    let rem_days = problem.problem.days - day in
    if rem_days > signup then begin
      Log.debug (fun fmt -> fmt "signing up");
      lib.signed_up_since <- day;
      let solved = solve_library problem lib rem_days in
      Log.debug (fun fmt -> fmt "solved library");
      solved :: (solve_problem problem (day + signup))
    end else []
  end

let solve problem =
  let problem = convert_problem problem in
  let solution = solve_problem problem 0 in
  Log.debug (fun fmt -> fmt "solution found");
  solution
