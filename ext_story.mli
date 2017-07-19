
type cf_experiment = Global_trace.t * Global_trace.t * ((int * Grid.constr * int) list) * Ext_tools.IntSet.t
(* (factual_subtrace, cf_subtrace, inhibition arrows, blocked events) *)
type extended_story = Global_trace.t * (cf_experiment list) (* (cumulated_factual_subtrace, counterfactual_experiments) *)

type inhibitions_finding_mode = Consider_entire_trace | Prefer_predicted_core | Consider_only_predicted_core
type configuration =
{
  compression_algorithm : Trace_explorer.t -> Causal_core.var_info_table -> int list -> int list;
  give_cumulated_core_to_heuristic : bool; (* Give cumulated core to blocking heuristic instead of initial core. *)
  heuristic    : Global_trace.t -> Ext_tools.IntSet.t -> int list -> int -> Resimulator_interface.interventions;
  nb_samples   : int;
  trace_scoring_heuristic : Global_trace.t -> Global_trace.t -> int list -> int -> int ;
  threshold    : float;
  max_counterfactual_exps : int;
  cf_inhibitions_finding_mode : inhibitions_finding_mode;
  (* Consider_only_precomputed_core generates shorter stories but invalid inhibition arrows when core predictions are wrong. *)
  fc_inhibitions_finding_mode : inhibitions_finding_mode;
  max_inhibitors_added_per_factual_events : int;
  max_inhibitors_added_per_cf_events : int;
  add_common_events_to_both_cores : bool; (* Generate longer stories, but permit to have a coherent and valid counterfactual experiment *)
  compute_inhibition_arrows_for_every_events : bool; (* Intensive & Generate longer stories ! *)
  adjust_inhibition_arrows_with_new_core_predictions : bool; (* Recommended if Consider_only_precomputed_core is used. *)
}

val compute_extended_story : Trace_explorer.t -> int -> configuration -> extended_story

val kaflow_compression : Trace_explorer.t -> Causal_core.var_info_table -> int list -> int list
