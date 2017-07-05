open Ext_tools

type step = Trace.step
type counterfactual_step = Resimulation.step

type blocked_f_event = int (*Simulation_info.story_id*)

type blocked_cf_event =
  | Blocked_rule of int * ASet.t * int option * int option (* rule_id * agents_involved (subset) * after_f_event * before_f_event *)
  | Blocked_pert of string * ASet.t * int option * int option (* pert_name * agents_involved (subset) * after_f_event * before_f_event *)

type interventions = blocked_f_event list * blocked_cf_event list

type stop_condition =
  | Time_limit of float
  | Event_has_happened of int (* Simulation_info.story_id *)
  | Event_has_not_happened of int (* Simulation_info.story_id *)
  | Rule_has_happened of int (* rule_id *)
  | Rule_has_not_happened of int (* rule_id *)
  | Obs_has_happened of string
  | Obs_has_not_happened of string
  | Any_event_not_happened

type stop_conditions = stop_condition list

type stop =
  | Continue | Stop_after | Stop_before

val is_cf_event_blocked : interventions -> int -> step -> bool

val is_f_event_blocked : interventions -> step -> bool

val must_stop : stop_conditions -> counterfactual_step -> stop

val resimulate : interventions -> stop_conditions -> Global_trace.t -> Global_trace.t
