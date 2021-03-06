open Ext_tools

type step = Trace.step
type counterfactual_step = Resimulation.step

type blocked_f_event = int (* index *)

type blocked_cf_event =
  | Blocked_rule of int * ASet.t * int option * int option (* rule_id * agents_modified (subset) * after_f_event * before_f_event *)
  | Blocked_step of ASet.t * int option * int option (* agents_modified (subset) * after_f_event * before_f_event *)

type interventions = blocked_f_event list * blocked_cf_event list

type stop_condition =
  | Time_limit of float
  | Event_has_happened of int (* index *)
  | Event_has_not_happened of int (* index *)
  | Rule_has_happened of int (* rule_id *)
  | Rule_has_not_happened of int (* rule_id *)
  | Obs_has_happened of string
  | Obs_has_not_happened of string
  | Any_event_not_happened

type stop_conditions = stop_condition list

type stop =
  | Continue | Stop_after | Stop_before

val is_cf_event_blocked : blocked_cf_event list -> int -> Model.t ->  int option -> (Instantiation.concrete Instantiation.action) list -> bool

val must_stop : stop_conditions -> int -> counterfactual_step -> stop

val resimulate : interventions -> stop_conditions -> Global_trace.t -> Global_trace.t

val print : Format.formatter -> interventions -> unit
val print_short : Format.formatter -> interventions -> unit
