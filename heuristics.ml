open Ext_tools
open Interface

(* TO AVOID INFINITE LOOPS, HEURISTICS MUST NOT BLOCK EVENTS THAT ARE IN THE CORE ! *)

(* Block in trace T every event that involve agents in the factual core and that are not in the factual causal core. *)
let heuristic_block_all_persistent trace core : interventions =
  let agents_involved_in_trace trace =
    let agents = List.map (fun s -> agents_tested_ts (step_to_ts s)) trace in
    List.fold_left ASet.union ASet.empty agents in
  
  let involved agents_involved step =
    let step_agents_tested = agents_tested_ts (step_to_ts step) in
    (step, ASet.inter step_agents_tested agents_involved) in

  let is_admissible_rule step =
    match step_to_ts step with
    | Trace.Rule _ -> List.forall (fun c -> c <> get_index step) core
    | _ -> false in

  let block_f_event (step, inv) = get_id step in

  let block_cf_events (step, inv) =
    match step_to_ts step with
    | Trace.Rule (rid,_,_) -> Blocked_rule (rid, inv, None, None)
    | _ -> assert false in

  let subtrace = core_to_subtrace trace core in
  let agents_involved_in_core = agents_involved_in_trace subtrace in
  let events_to_block = List.filter is_admissible_rule events_to_block in
  let events_to_block = List.map (involved agents_involved_in_core) events_to_block in
  let events_to_block = List.filter (fun (s,inv) -> not (ASet.is_empty inv)) events_to_block in
  (List.map block_f_event events_to_block, List.map block_cf_events events_to_block)

let heuristic_block_all trace core : interventions =
  let (f,_) = heuristic_block_all_persistent trace core
  in (f,[])
