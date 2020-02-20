module Log = (val Logger.create "common.solver" : Logs.LOG)

type t =
  { name : string ;
    repeatable : bool ;
    persistent_configurator : unit -> unit ;
    configurator : unit -> unit ;
    solver : Problem.t -> Solution.t }

let make ~name ?repeatable ?configurator ?persistent_configurator solver =
  let repeatable =
    match repeatable, configurator, persistent_configurator with
    | Some repeatable, _, _ -> repeatable
    | _, Some _, _ | _, _, Some _ -> true
    | _ -> false
  in
  let configurator =
    match configurator with
    | None -> (fun () -> Random.self_init ())
    | Some configurator -> configurator
  in
  let persistent_configurator =
    match persistent_configurator with
    | None -> (fun () -> ())
    | Some persistent_configurator -> persistent_configurator
  in
  { name ; repeatable ; configurator ; persistent_configurator ; solver }

let name solver = solver.name
let is_repeatable solver = solver.repeatable
let configure solver = solver.configurator ()
let persistent_configure solver = solver.persistent_configurator ()
let solve solver = solver.solver
