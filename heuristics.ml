open Ext_tools
open Interface

let agents_involved_in_trace trace =
  let agents = List.map (fun s -> agents_tested_ts (step_to_ts s)) trace in
  List.sort_uniq Agent.compare (List.flatten agents)

 (* For example :
    - Block permanently in trace T every event that involve agents in the factual core and that are not in the factual causal core.
    - Block permanently an event that is suspected to have an impact later.*)
let heuristic_choose_interventions trace core : interventions =
  let subtrace = core_to_subtrace trace core in
  let agents_involved_in_core = agents_involved_in_trace subtrace in
  [] (* TODO *)
