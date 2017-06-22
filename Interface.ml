open Trace.Simulation_info
open Trace

type blocked_event =
  | One_time of int (* Simulation_info.story_event *)
  | Every_instance of int * Agent.t list option * float option * float option (* rule_id * agents_involved * from_time * until_time *)

type interventions = blocked_event list

type stop_condition =
  | Time_limit of float
  | Event_has_happened of int (* Simulation_info.story_event *)
  | Event_has_not_happened of int (* Simulation_info.story_event *)
  | Rule_has_happened of int (* rule_id *)
  | Rule_has_not_happened of int (* rule_id *)
  | Any_event_not_happened

type stop_conditions = stop_condition list


type counterfactual_step =
  | Factual_event_happened of step
  | Factual_event_blocked of step
  | Counterfactual_event_happened of step

type blocked_predicate = step -> bool

type stop =
  | Continue | Stop_after | Stop_before
type stop_predicate = counterfactual_step -> stop


let step_info step = match step with
  | Rule (_,_,i) | Pert (_,_,i) | Obs (_,_,i) -> Some i
  | _ -> None

let agents_involved instantiation =
  let aggregate_agent acc test = match test with
  | Instantiation.Is_Here a -> a::acc
  | _ -> acc
  in List.sort_uniq Agent.compare (List.fold_left aggregate_agent [] (List.flatten instantiation.Instantiation.tests))

let list_included lst1 lst2 =
  let lst1 = List.sort_uniq compare lst1
  and lst2 = List.sort_uniq compare lst2 in
  let rec aux lst1 lst2 = match lst1, lst2 with
    | [], _ -> true
    | _, [] -> false
    | h1::tl1, h2::tl2 when h1=h2 -> aux tl1 tl2
    | h1::tl1, h2::tl2 when h1<h2 -> false
    | h1::tl1, h2::tl2 -> aux (h1::tl1) tl2
  in aux lst1 lst2

let rec interventions_to_predicate interv step =
  match interv with
  | [] -> false
  | (One_time ev)::lst ->
  (
    match step_info step with
    | Some info -> if info.story_event = ev then true else interventions_to_predicate lst step
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
        | Some ags -> if list_included (agents_involved inst) ags then true else interventions_to_predicate lst step
      )
    )
    | _ -> interventions_to_predicate lst step
  )

let rec stop_conditions_to_predicate scs cstep =
  match scs, cstep with
  | [], _ -> Continue
  | (Time_limit t)::lst, Factual_event_happened step
  | (Time_limit t)::lst, Factual_event_blocked step
  | (Time_limit t)::lst, Counterfactual_event_happened step ->
  (
    match step_info step with
    | Some i when i.story_time > t -> Stop_before
    | _ -> stop_conditions_to_predicate lst cstep
  )
  | (Any_event_not_happened)::lst, Factual_event_blocked _ -> Stop_after
  | (Event_has_not_happened ev)::lst, Factual_event_blocked step
  | (Event_has_happened ev)::lst, Factual_event_happened step ->
  (
    match step_info step with
    | Some i when i.story_event = ev -> Stop_after
    | _ -> stop_conditions_to_predicate lst cstep
  )
  | (Rule_has_not_happened id_rule)::lst, Factual_event_blocked step
  | (Rule_has_happened id_rule)::lst, Factual_event_happened step
  | (Rule_has_happened id_rule)::lst, Counterfactual_event_happened step ->
  (
    match step with
    | Rule(id,_,_) when id = id_rule -> Stop_after
    | _ -> stop_conditions_to_predicate lst cstep
  )
  | hd::lst, _ -> stop_conditions_to_predicate lst cstep

let resimulate model trace blocked_pred stop_pred = []
