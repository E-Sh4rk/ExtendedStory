open Ext_tools

type step = Trace.step
type counterfactual_step = Resimulation.step

type blocked_event =
  | One_time of int (* Simulation_info.story_id *)
  | Every_instance of int * ASet.t * float option * float option (* rule_id * agents_involved (subset) * from_time * until_time *)

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

val interventions_to_predicate : interventions -> step -> bool

val stop_conditions_to_predicate : stop_conditions -> counterfactual_step -> stop

val resimulate : Model.t -> block_predicate -> stop_predicate -> Trace.t -> counterfactual_step list
