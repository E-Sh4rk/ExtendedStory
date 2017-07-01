open Ext_tools
open Interface

let agents_involved_in_trace trace =
  let agents = List.map (fun s -> agents_tested_ts (step_to_ts s)) trace in
  List.sort_uniq Pervasives.compare (List.flatten agents)

(* Block permanently in trace T every event that involve agents in the factual core and that are not in the factual causal core. *)
let heuristic_block_all trace core : interventions =
  let involved step agents_involved =
    let step_agents_tested = agents_tested_ts (step_to_ts step) in
    intersection_not_empty step_agents_tested agents_involved in

  let rec aux trace agents_involved core = match trace, core with
  | [], _ -> []
  | s::trace, [] -> if involved s agents_involved then s::(aux trace agents_involved []) else aux trace agents_involved []
  | s::trace, i::core when get_index s = i -> aux trace agents_involved core
  | s::trace, core -> if involved s agents_involved then s::(aux trace agents_involved core) else aux trace agents_involved core in

  let subtrace = core_to_subtrace trace core in
  let agents_involved_in_core = agents_involved_in_trace subtrace in
  let events_to_block = aux trace agents_involved_in_core core in
  [] (* TODO *)
