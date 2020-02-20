open Common
open Problem
module Log = (val Logger.create "solver.niols" : Logs.LOG)

let scanable_books_for_library days library =
  library.books_per_day * max 0 (days - library.signup_time)

let score_library days library =
  (* Log.debug (fun m -> m "Computing score for library %d" library.lid); *)
  let nb_books = ref (scanable_books_for_library days library) in
  let score = ref 0 in
  let i = ref 0 in
  while !nb_books > 0 && !i < Array.length library.content do
    let book = library.content.(!i) in
    if not book.selected then
      (score := book.score + !score;
       decr nb_books);
    incr i
  done;
  (* Log.debug (fun m -> m "Library %d has score %d" library.lid !score); *)
  !score

let find_best_library days libraries =
  let best_score = ref (-1) in
  let best_library = ref (Obj.magic 0) in (* null *)
  libraries |> Array.iter (fun library ->
      if Obj.magic library <> 0 then (* null *)
        (
          let score = score_library days library in
          if score > !best_score then
            (
              best_score := score;
              best_library := library;
            )
        )
    );
  if Obj.magic !best_library = 0 || !best_score = 0 then (* null *)
    None
  else
    Some !best_library

let select_scanable_books days library =
  let nb_books = ref (scanable_books_for_library days library) in
  let books = ref [] in
  let i = ref 0 in
  while !nb_books > 0 && !i < Array.length library.content do
    let book = library.content.(!i) in
    if not book.selected then
      (book.selected <- true;
       books := book :: !books;
       decr nb_books);
    incr i
  done;
  !books (* FIXME: List.rev probablement pas nécessaire *)

let solve problem =
  let rec solve days =
    if days <= 0 then
      []
    else
      (
        Log.debug (fun m -> m "%d days remaining" days);
        match find_best_library days problem.libraries with
        | None -> []
        | Some library ->
          problem.libraries.(library.lid) <- Obj.magic 0; (* null *)
          let books = select_scanable_books days library in
          (library, books) :: solve (days - library.signup_time)
      )
  in
  solve problem.days
