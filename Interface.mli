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

type block_predicate = step -> bool

type stop =
  | Continue | Stop_after | Stop_before
type stop_predicate = counterfactual_step -> stop

val interventions_to_predicate : interventions -> step -> bool

val stop_conditions_to_predicate : stop_conditions -> counterfactual_step -> stop

val resimulate : Model.t -> block_predicate -> stop_predicate -> Trace.t -> counterfactual_step list
