open Trace.Simulation_info

(* ----- Utils ----- *)

module Int = struct
  type t = int
  let compare = Pervasives.compare
end
module IntSet = Set.Make(Int)

let logs s = print_string s ; print_newline () ; flush stdout

let min_c c e1 e2 = match c e1 e2 with
| -1 -> e1 | 0 -> e1 | 1 -> e2 | _ -> assert false
let max_c c = min_c (fun a b -> - (c a b))

let list_max_c c lst = List.fold_left (max_c c) (List.hd lst) lst
let list_min_c c lst = List.fold_left (min_c c) (List.hd lst) lst

let rec cut_after_index i lst = match i, lst with
  | _, [] -> []
  | 0, s::lst -> [s]
  | n, s::lst -> s::(cut_after_index (n-1) lst)

(* ----- Kappa ----- *)

module ASet = Set.Make(Agent)

let srule_id_from_rule_id env rid = (Model.get_rule env rid).Primitives.syntactic_rule

let rule_ast_name env rule_id = 
  Format.asprintf "%a" 
    (Model.print_ast_rule ~env) 
    (srule_id_from_rule_id env rule_id)

let get_name model (i,step) default = match step with
  | Trace.Rule (rule_id,inst,infos) -> rule_ast_name model rule_id
  | Trace.Obs (name,inst,infos) -> name
  | Trace.Pert (name,inst,infos) -> name
  | _ -> default

let agents_tested tests =
  let aggregate_agent acc test = match test with
  | Instantiation.Is_Here a -> ASet.add a acc
  | _ -> acc
  in List.fold_left aggregate_agent ASet.empty (List.flatten tests)

let agents_tested_ts ts = match ts with
  | Trace.Rule (_,inst,_) | Trace.Pert (_,inst,_)
  -> agents_tested inst.Instantiation.tests
  | Trace.Obs (_,tests,_) -> agents_tested tests
  | Trace.Init _ -> ASet.empty
  | _ -> ASet.empty

(* ----- Specific types ----- *)

type step = Causal_core_shared.step_id * Trace.step

let set_index i (_,s) = (i,s)
let get_index (i,_) = i

let get_id_of_ts step = match step with
  | Trace.Rule (_,_,infos) | Trace.Pert (_,_,infos) | Trace.Obs (_,_,infos)
  -> infos.story_id
  | _ -> raise Not_found

let set_id_of_ts id step = match step with
  | Trace.Rule (rid,inst,infos) -> Trace.Rule (rid,inst,{infos with story_id=id})
  | Trace.Pert (str,inst,infos) -> Trace.Pert (str,inst,{infos with story_id=id})
  | Trace.Obs (str,inst,infos) -> Trace.Obs (str,inst,{infos with story_id=id})
  | s -> s

let get_id (i,s) = match s with
  | Trace.Rule _ | Trace.Pert _ | Trace.Obs _ -> get_id_of_ts s
  | Trace.Init _ -> i
  | _ -> failwith "Invalid trace !"

let set_id id step = match step with
  | (i, Trace.Rule _) | (i, Trace.Pert _) | (i, Trace.Obs _) -> (i, set_id_of_ts id (snd step))
  | (_, Trace.Init inst) -> (id, Trace.Init inst)
  | _ -> failwith "Invalid trace !"

let get_time_of_ts ts default = match ts with
  | Trace.Rule (_,_,infos) | Trace.Pert (_,_,infos) | Trace.Obs (_,_,infos)
  -> infos.story_time
  | Trace.Init _ -> 0.0
  | _ -> default

let get_time (i,ts) default = get_time_of_ts ts default

let ts_to_step i ts = (i, ts)
let step_to_ts (i,ts) = ts

let trace_to_ttrace trace = List.map step_to_ts trace
let ttrace_to_trace ttrace = List.mapi ts_to_step ttrace

let ctrace_to_trace ctrace =
  let is_happenning_event cs = match cs with
  | Resimulation.Factual_happened _ | Resimulation.Counterfactual_happened _ -> true
  | Resimulation.Factual_did_not_happen _ -> false in
  let regularize_event i cs = match cs with
  | Resimulation.Factual_happened ts ->  ts_to_step i ts
  | Resimulation.Counterfactual_happened ts -> ts_to_step i ts
  | _ -> assert false in
  let ctrace = List.filter is_happenning_event ctrace in
  List.mapi regularize_event ctrace

let index_to_id trace index =
  get_id (List.nth trace index)

let rec nb_of_events_before_time_strict trace time = match trace with
  | [] -> 0
  | s::trace when get_time s 0.0 >= time -> 0
  | s::trace -> 1 + (nb_of_events_before_time_strict trace time)

let rec nb_of_events_before_time_large trace time = match trace with
  | [] -> 0
  | s::trace when get_time s 0.0 > time -> 0
  | s::trace -> 1 + (nb_of_events_before_time_large trace time)

let same_sign x y = match x, y with
| n, m when n >= 0 && m < 0 -> false
| n, m when n < 0 && m >= 0 -> false
| n, m -> true

let get_event id trace =
  let rec aux trace index = match id, trace with
  | _, [] -> None
  | _, s::trace when get_id s = id -> Some (index, s)
  | n, s::trace when not (same_sign n (get_id s)) -> aux trace (index+1)
  | n, s::trace when n >= 0 && get_id s > id -> None
  | n, s::trace when n < 0 && get_id s < id -> None
  | _, s::trace -> aux trace (index+1)
  in aux trace 0

let rec cut_after id trace = match trace with
  | [] -> []
  | s::trace when get_id s = id -> [s]
  | s::trace -> s::(cut_after id trace)

let rec core_to_subtrace trace core = match core, trace with
  | [], _ -> []
  | index::core, s::trace when get_index s = index -> s::(core_to_subtrace trace core)
  | core, s::trace -> core_to_subtrace trace core
  | _, _ -> assert false

  let rec core_to_subtrace_diff trace core = match trace, core with
  | trace, [] -> trace
  | s::trace, i::core when get_index s = i -> core_to_subtrace_diff trace core
  | s::trace, core -> s::(core_to_subtrace_diff trace core)
  | _, _ -> assert false
