open ExtPervasives
module Log = (val Logger.create "common.problem" : Logs.LOG)

type book =
  { bid : int ;
    score : int ;
    mutable selected : bool }
[@@deriving show]

type library =
  { lid : int ;
    content : book array ;
    content_by_bid : (int, int) Hashtbl.t [@opaque]; (* indirection table *)
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

let rec list_hdn n l =
  if n = 0 then []
  else match l with
    | [] -> []
    | h::q -> h :: list_hdn (n-1) q

let analyse problem =
  (* Log.debug (fun m -> m "\n%a" pp problem); *)
  Log.info (fun m -> m "This is the analysis of problem `%s`." (name problem));
  Log.info (fun m -> m "Books: %d" (Array.length problem.books));
  Log.info (fun m -> m "Libraries: %d" (Array.length problem.libraries));
  Log.info (fun m -> m "Days: %d" problem.days);
  Log.info (fun m -> m "10 top libraries per nb of books: %s"
               (problem.libraries
                |> Array.to_list
                |> List.sort (fun l1 l2 -> -compare (Array.length l1.content) (Array.length l2.content))
                |> list_hdn 10
                |> List.map (fun l -> soi (Array.length l.content))
                |> String.concat " "));
  Log.info (fun m -> m "10 top libraries per books-per-day: %s"
               (problem.libraries
                |> Array.to_list
                |> List.sort (fun l1 l2 -> -compare l1.books_per_day l2.books_per_day)
                |> list_hdn 10
                |> List.map (fun l -> (soi l.books_per_day) ^ "/" ^ (soi l.signup_time))
                |> String.concat " "));
  Log.info (fun m -> m "10 top libraries per signup-time: %s"
               (problem.libraries
                |> Array.to_list
                |> List.sort (fun l1 l2 -> -compare l1.signup_time l2.signup_time)
                |> list_hdn 10
                |> List.map (fun l -> (soi l.books_per_day) ^ "/" ^ (soi l.signup_time))
                |> String.concat " "));
  Log.info (fun m -> m "-----")

(* Parsing *)

let from_channel ~name (ichan : in_channel) : t =
  Log.debug (fun m -> m "Parsing problem: %s" name);
  match input_line ichan |> String.split_on_char ' ' with
  | [nb_books; nb_libraries; days] ->
    (
      Log.debug (fun m -> m "Getting books");
      let books =
        input_line ichan
        |> String.split_on_char ' '
        |> List.map ios
        |> List.mapi (fun bid score -> {bid; score; selected=false})
        |> Array.of_list
      in
      let nb_books = ios nb_books in
      assert (Array.length books = nb_books);
      Log.debug (fun m -> m "Getting libraries");
      let libraries =
        let libraries = ref [] in
        for lid = 0 to ios nb_libraries - 1 do
          match input_line ichan |> String.split_on_char ' ' with
          | [nb_content; signup_time; books_per_day] ->
            (
              let content =
                input_line ichan
                |> String.split_on_char ' '
                |> List.map (fun bid -> books.(ios bid))
                |> List.sort (fun b1 b2 -> - compare b1.score b2.score) (* Sort from biggest to smallest *)
                |> Array.of_list
              in
              assert (Array.length content = ios nb_content);
              let content_by_bid = Hashtbl.create (Array.length content) in
              content |> Array.iteri (fun i book ->
                  Hashtbl.add content_by_bid book.bid i
                );
              let signup_time = ios signup_time in
              let books_per_day = ios books_per_day in
              libraries := { lid; content; content_by_bid; signup_time; books_per_day } :: !libraries
            )
          | _ -> assert false
        done;
        !libraries
        |> List.rev
        |> Array.of_list
      in
      let days = ios days in
      Log.debug (fun m -> m "Returning");
      { name; books; libraries; days }
    )
  | _ -> assert false

let from_file (filename : string) : t =
  let name = Filename.basename filename in
  let ichan = open_in filename in
  let problem = from_channel ~name ichan in
  close_in ichan;
  problem
