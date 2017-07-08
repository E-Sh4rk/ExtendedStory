open Ext_tools
open Resimulator_interface
open Global_trace

(* TO AVOID INFINITE LOOPS, HEURISTICS MUST NOT BLOCK EVENTS THAT ARE IN THE CORE ! *)
type persistence = No_persistence | Persistence | Full_persistence

let admissible_events trace core eoi : int list = 
  let is_admissible_rule core step index = match step with
  | Trace.Rule _ -> not (IntSet.mem index core)
  | _ -> false
  in
  let rec events_admissible core acc i = match i with
  | i when i < 0 -> acc
  | i -> if is_admissible_rule core (get_step trace i) i then events_admissible core (i::acc) (i-1) else events_admissible core acc (i-1)
  in
  events_admissible (IntSet.of_list core) [] (eoi-1)

let next_core_event_that_test_an_agent_of trace core i agents =
  let rec aux core = match core with
  | [] -> None
  | i::core -> let agents_tested = agents_involved (get_tests trace i) in
  let inter = ASet.inter agents agents_tested in
  if ASet.is_empty inter then aux core else Some i
  in
  let core = List.filter (fun n -> n > i) core in
  let core = List.sort_uniq Pervasives.compare core in
  aux core

(* TODO : improvement : block only events that have an action on a logical site NOT tested by a core event
but on an agent tested by this core event, and only if it is the last event to modify this site before the core event. *)
let heuristic_1 trace core eoi : interventions =

  let find_next_admissible_core_event i action = None in

  let find_next_admissible_core_events i =
    let evs = List.map (find_next_admissible_core_event i) (get_actions trace i) in
    let evs = List.filter (fun opt -> opt <> None) evs in
    List.map (function None -> assert false | Some i -> i) evs in

  let admissible = admissible_events trace core eoi in
  let admissible = List.map find_next_admissible_core_events admissible in
  ([],[])

(* Block in trace T every event that involve agents in the factual core and that are not in the factual causal core. *)
let heuristic_block_all pers trace core eoi : interventions =

  let involved agents_tested i =
    let modified = agents_involved (get_actions trace i) in
    (i, ASet.inter agents_tested modified) in

  let block_f_event (i, _) = i in

  let block_cf_events (i, inv) =
    match get_step trace i with
    | Trace.Rule (rid,_,_) -> Blocked_rule (rid, inv, None, next_core_event_that_test_an_agent_of trace core i inv)
    | _ -> assert false in

  let agents_tested_in_core = List.fold_left
  (fun acc i -> ASet.union acc (agents_involved (get_tests trace i))) ASet.empty core in
  let events_to_block = admissible_events trace core eoi in
  let events_to_block = List.map (involved agents_tested_in_core) events_to_block in
  let events_to_block = List.filter (fun (i,inv) -> not (ASet.is_empty inv)) events_to_block in
  let full_pers = (List.map block_f_event events_to_block, List.map block_cf_events events_to_block) in
  if pers = Persistence then full_pers
  else if pers = No_persistence then let (f,_) = full_pers in (f,[])
  else let (f,cf) = full_pers in
  (f,List.map (function Blocked_rule (rid,inv,_,_) -> Blocked_rule (rid,inv,None,None) | _ -> assert false) cf)
