open Ext_tools
open Interface
open Global_trace

(* TO AVOID INFINITE LOOPS, HEURISTICS MUST NOT BLOCK EVENTS THAT ARE IN THE CORE ! *)

(* Block in trace T every event that involve agents in the factual core and that are not in the factual causal core. *)
let heuristic_block_all_persistent trace core : interventions =

  let is_admissible_rule step index = match step with
    | Trace.Rule _ -> List.for_all (fun c -> c <> index) core
    | _ -> false in

  let rec events_admissible acc i = match i with
  | i when i < 0 -> acc
  | i -> let s = get_step trace i in if is_admissible_rule s i then events_admissible (i::acc) (i-1) else events_admissible acc (i-1)
  in

  let involved agents_tested i =
    let modified = agents_involved (get_actions trace i) in
    (i, ASet.inter agents_tested modified) in

  let block_f_event (i, inv) = i in

  let block_cf_events (i, inv) =
    match get_step trace i with
    | Trace.Rule (rid,_,_) -> Blocked_rule (rid, inv, None, None)
    | _ -> assert false in

  let agents_tested_in_core = List.fold_left
  (fun acc i -> ASet.union acc (agents_involved (get_tests trace i))) ASet.empty core in
  let events_to_block = events_admissible [] ((length trace)-1) in
  let events_to_block = List.map (involved agents_tested_in_core) events_to_block in
  let events_to_block = List.filter (fun (i,inv) -> not (ASet.is_empty inv)) events_to_block in
  (List.map block_f_event events_to_block, List.map block_cf_events events_to_block)

let heuristic_block_all trace core : interventions =
  let (f,_) = heuristic_block_all_persistent trace core
  in (f,[])
