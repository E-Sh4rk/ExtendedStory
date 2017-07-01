open Ext_tools
open Trace.Simulation_info
open Resimulation
open Trace

type counterfactual_step = Resimulation.step
type step = Trace.step

type blocked_event =
  | One_time of int (* Simulation_info.story_id *)
  | Every_instance of int * Agent.t list option * float option * float option (* rule_id * agents_involved (subset) * from_time * until_time *)

type interventions = blocked_event list

type stop_condition =
  | Time_limit of float
  | Event_has_happened of int (* Simulation_info.story_id *)
  | Event_has_not_happened of int (* Simulation_info.story_id *)
  | Rule_has_happened of int (* rule_id *)
  | Rule_has_not_happened of int (* rule_id *)
  | Any_event_not_happened

type stop_conditions = stop_condition list

type block_predicate = step -> bool

type stop =
  | Continue | Stop_after | Stop_before
type stop_predicate = counterfactual_step -> stop


let step_info step = match step with
  | Rule (_,_,i) | Pert (_,_,i) | Obs (_,_,i) -> Some i
  | _ -> None

let rec interventions_to_predicate interv step =
  match interv with
  | [] -> false
  | (One_time ev)::lst ->
  (
    match step_info step with
    | Some info -> if info.story_id = ev then true else interventions_to_predicate lst step
    | None -> interventions_to_predicate lst step
  )
  | (Every_instance (rule_id, involved, from_time, to_time))::lst ->
  (
    match step with
    | Rule (id, inst, info) ->
    (
      let from_time = match from_time with None -> info.story_time | Some t -> t
      and to_time = match to_time with None -> info.story_time | Some t -> t in
      if rule_id <> id || info.story_time > to_time || info.story_time < from_time then interventions_to_predicate lst step else
      (
        match involved with
        | None -> true
        | Some ags -> if list_included ags (agents_tested inst.Instantiation.tests) then true else interventions_to_predicate lst step
      )
    )
    | _ -> interventions_to_predicate lst step
  )

let rec stop_conditions_to_predicate scs cstep =
  match scs, cstep with
  | [], _ -> Continue
  | (Time_limit t)::lst, Factual_happened step
  | (Time_limit t)::lst, Factual_did_not_happen (_, step)
  | (Time_limit t)::lst, Counterfactual_happened step ->
  (
    match step_info step with
    | Some i when i.story_time > t -> Stop_before
    | _ -> stop_conditions_to_predicate lst cstep
  )
  | (Any_event_not_happened)::lst, Factual_did_not_happen _ -> Stop_after
  | (Event_has_not_happened ev)::lst, Factual_did_not_happen (_, step)
  | (Event_has_happened ev)::lst, Factual_happened step ->
  (
    match step_info step with
    | Some i when i.story_id = ev -> Stop_after
    | _ -> stop_conditions_to_predicate lst cstep
  )
  | (Rule_has_not_happened id_rule)::lst, Factual_did_not_happen (_, step)
  | (Rule_has_happened id_rule)::lst, Factual_happened step
  | (Rule_has_happened id_rule)::lst, Counterfactual_happened step ->
  (
    match step with
    | Rule(id,_,_) when id = id_rule -> Stop_after
    | _ -> stop_conditions_to_predicate lst cstep
  )
  | hd::lst, _ -> stop_conditions_to_predicate lst cstep

let resimulate model block_pred stop_pred trace =
  let last_time = ref 0.0 in
  let next_event lst = match lst with
  | [] -> None
  | s::lst -> begin match step_info s with
    | None -> Some (s, !last_time, block_pred s)
    | Some info -> last_time := info.story_time ; Some (s, info.story_time, block_pred s)
  end in
  let rec resimulate_step next_events state acc =
    let (consummed, cstep_opt, state) = Resimulation.do_step (next_event next_events) state in
    let next_events = if consummed then List.tl next_events else next_events in
    try
    (
      match cstep_opt with
      | None -> resimulate_step next_events state acc
      | Some s when stop_pred s = Continue -> resimulate_step next_events state (s::acc)
      | Some s when stop_pred s = Stop_after -> s::acc
      | Some s -> acc
    )
    with Resimulation.End_of_resimulation -> acc
  in List.rev (resimulate_step trace (Resimulation.init model (Random.get_state ())) [])
