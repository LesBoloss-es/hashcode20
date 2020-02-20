let pf = Format.printf
let epf = Format.eprintf
let fpf = Format.fprintf
let spf = Format.sprintf

let soi = string_of_int
let ios = int_of_string

let (||>) f g = fun x -> f x |> g

let datetime () =
  let tm = Unix.(localtime (gettimeofday ())) in
  spf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (1900 + tm.tm_year) (1 + tm.tm_mon) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec

let (>>=) = Lwt.bind

let union_sorted l1 l2 = 
  let rec aux acc l1 l2 = 
    match l1, l2 with
    | [], [] -> List.rev acc
    | h1::t1, [] -> aux (h1::acc) t1 []
    | [], h2::t2 -> aux (h2::acc) [] t2
    | h1::t1,h2::t2 ->
      if h1 < h2 then
        aux (h1::acc) t1 l2
      else if h1 > h2 then
        aux (h2::acc) l1 t2
      else
        aux (h1::acc) t1 t2
  in
  aux [] l1 l2

let rec common_elts l1 l2 = 
  match l1, l2 with
  | _, [] -> 0
  | [], _ -> 0
  | h1::t1, h2::t2 ->
    if h1 < h2 then
      common_elts t1 l2
    else if h1 > h2 then
      common_elts l1 t2
    else
      1 + (common_elts t1 t2)

let rec non_common_elts l1 l2 = 
  match l1, l2 with
  | l1, [] -> List.length l1
  | [], _ -> 0
  | h1::t1, h2::t2 ->
    if h1 < h2 then
      1 + non_common_elts t1 l2
    else if h1 > h2 then
      non_common_elts l1 t2
    else
      non_common_elts t1 t2
