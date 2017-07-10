
type cf_part = Global_trace.t * ((int * Grid.constr * int) list) (* (subtrace, inhibition arrows) *)
type extended_story = Global_trace.t * (cf_part list) (* (subtrace, counterfactual parts) *)

type configuration =
{
  compression_algorithm : Trace_explorer.t -> Causal_core.var_info_table -> int list -> int list;
  heuristic    : Global_trace.t -> Ext_tools.IntSet.t -> int list -> int -> Resimulator_interface.interventions;
  nb_samples   : int;
  trace_rejection_heuristic : Global_trace.t -> Global_trace.t -> int list -> int -> bool ;
  threshold    : float;
  max_counterfactual_parts : int;
  keep_rejected_cf_parts : bool;
  precompute_cores : bool; (* Improve inhibition searching results. *)
  max_cf_inhibition_arrows : int;
  max_fc_inhibition_arrows_per_inhibator : int;
  add_all_factual_events_involved_to_factual_core : bool;
}

val compute_extended_story : Trace_explorer.t -> int -> configuration -> extended_story

val kaflow_compression : Trace_explorer.t -> Causal_core.var_info_table -> int list -> int list
