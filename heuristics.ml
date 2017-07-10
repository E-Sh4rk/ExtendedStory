open Ext_tools
open Resimulator_interface
open Global_trace

(* HEURISTICS SHOULD NOT BLOCK EVENTS THAT ARE IN THE BLACKLIST ! *)
type persistence = No_persistence | Persistence | Full_persistence

let admissible_events trace blacklist eoi : int list = 
  let is_admissible_rule step index = match step with
  | Trace.Rule _ -> not (IntSet.mem index blacklist)
  | _ -> false
  in
  let rec events_admissible acc i = match i with
  | i when i < 0 -> acc
  | i -> if is_admissible_rule (get_step trace i) i then events_admissible (i::acc) (i-1) else events_admissible acc (i-1)
  in
  events_admissible [] (eoi-1)

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

(* Block only events not in the factual causal core and that have an action on a logical site NOT TESTED by a core event
   but on an agent TESTED by this core event, and only if it is the last event to modify this site before the core event. *)
let heuristic_1 pers trace blacklist core eoi : interventions =

  let find_next_admissible_core_event i action =
    let agent = agents_involved [action] in
    let next = next_core_event_that_test_an_agent_of trace core i agent in
    match next with
    | None -> None
    | Some j -> let Grid.Constr (var, _) = action in
    if List.exists (fun (Grid.Constr (var', _)) -> Grid.Var var = Grid.Var var') (get_tests trace j)
    then None else
    (
      let history = get_history trace var in
      match History.last_before j history with
      | None -> assert false
      | Some g when g = i -> Some (j,agent)
      | Some g when g > i -> None
      | _ -> assert false
    )
  in

  let find_next_admissible_core_events i =
    let evs = List.map (find_next_admissible_core_event i) (get_actions trace i) in
    let evs = List.filter (fun opt -> opt <> None) evs in
    (i,List.map (function None -> assert false | Some i -> i) evs)
  in

  let block_cf_events (i, lst) =
    List.map (fun (j,agent) -> Blocked_rule (get_rule_id (get_step trace i) (-1), agent, None, Some j) ) lst
  in

  let admissible = admissible_events trace (IntSet.union blacklist (IntSet.of_list core)) eoi in
  let admissible = List.map find_next_admissible_core_events admissible in
  let admissible = List.filter (fun (_,l) -> l <> []) admissible in
  let persistent = (List.map (fun (i,_) -> i) admissible,List.flatten (List.map block_cf_events admissible)) in
  if pers = Persistence then persistent
  else if pers = No_persistence then let (f,_) = persistent in (f,[])
  else let (f,cf) = persistent in
  (f,List.map (function Blocked_rule (rid,inv,_,_) -> Blocked_rule (rid,inv,None,None) | _ -> assert false) cf)

(* Block in trace T every event that involve agents in the factual core and that are not in the factual causal core. *)
let heuristic_block_all pers trace blacklist core eoi : interventions =

  let involved agents_tested i =
    let modified = agents_involved (get_actions trace i) in
    (i, ASet.inter agents_tested modified) in

  let block_f_event (i, _) = i in

  let block_cf_events (i, inv) =
    Blocked_rule (get_rule_id (get_step trace i) (-1), inv, None, next_core_event_that_test_an_agent_of trace core i inv)
  in

  let agents_tested_in_core = List.fold_left (fun acc i -> ASet.union acc (agents_involved (get_tests trace i))) ASet.empty core in
  let events_to_block = admissible_events trace (IntSet.union blacklist (IntSet.of_list core)) eoi in
  let events_to_block = List.map (involved agents_tested_in_core) events_to_block in
  let events_to_block = List.filter (fun (i,inv) -> not (ASet.is_empty inv)) events_to_block in
  let persistent = (List.map block_f_event events_to_block, List.map block_cf_events events_to_block) in
  if pers = Persistence then persistent
  else if pers = No_persistence then let (f,_) = persistent in (f,[])
  else let (f,cf) = persistent in
  (f,List.map (function Blocked_rule (rid,inv,_,_) -> Blocked_rule (rid,inv,None,None) | _ -> assert false) cf)
