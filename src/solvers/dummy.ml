open Common
module Log = (val Logger.create "solvers.dummy" : Logs.LOG)

let texts = ["Yay o/"; "Hurrah!"]
let curr_texts = ref texts

let configure () =
  curr_texts  := List.tl !curr_texts;
  if !curr_texts = [] then
    curr_texts := texts

let dummy (_problem : Problem.t) : Solution.t =
  Log.debug (fun m -> m "Dummy solver. %s" (List.hd !curr_texts));
  assert false
