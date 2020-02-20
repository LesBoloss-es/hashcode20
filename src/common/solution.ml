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

let rec list_hdn n l =
  if n = 0 then []
  else match l with
    | [] -> []
    | h::q -> h :: list_hdn (n-1) q

let rec list_tln n l =
  if n = 0 then l
  else match l with
    | [] -> []
    | _::q -> list_tln (n-1) q

let score problem solution =
  let signed_up_libraries = ref [] in
  let non_signed_up_libraries = ref solution in
  let current_signing_up = ref 0 in
  let scanned_books = ref [] in
  for _day = 0 to problem.days do
    (* For all signed-up libraries, add their books. *)
    signed_up_libraries :=
      !signed_up_libraries |> List.map (fun (library, books) ->
          list_hdn library.books_per_day books |> List.iter (fun book ->
              scanned_books := book :: !scanned_books
            );
          (library, list_tln library.books_per_day books)
        );
    (* Move forward the first non-signed up library. *)
    incr current_signing_up;
    match !non_signed_up_libraries with
    | [] -> ()
    | (library, books) :: other_libraries ->
      if library.signup_time >= !current_signing_up then
        (
          non_signed_up_libraries := other_libraries;
          signed_up_libraries := (library, books) :: !signed_up_libraries;
          current_signing_up := 0
        )
  done;
  (* Count uniq books *)
  !scanned_books
  |> List.sort_uniq compare
  |> List.fold_left (fun score book -> score + book.score) 0
