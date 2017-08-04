

(** A counterfactual experiment represented by :
the factual trace, the counterfactual trace (synchronized with the factual),
a set of inhibition arrows and a set of blocked events. *)
type cf_experiment = Global_trace.t * Global_trace.t * Ext_tools.InhSet.t * Ext_tools.IntSet.t

(** An extended story represented by : the initial story computed,
a story extended with important events (can be used for dynamic rendering),
a cumulative story that contains every factual events involved (can be used for static rendering)
and a list of counterfactual experiments.
*)
type extended_story = Global_trace.t * Global_trace.t * Global_trace.t * (cf_experiment list)

(**
Indicates which trace are used to search the destinations of inhibition arrows :
Consider_entire_trace -> use the whole trace
Prefer_predicted_core -> use the whole trace but prefer browsing events of the current core
Consider_only_predicted_core -> use only events of the core except when this is not suficient

The last option can generate invalid inhibition arrows if the core evolve.
It is advised to use it with adjust_inhibition_arrows_with_new_core_predictions set to true.
*)
type inhibitions_finding_mode = Consider_entire_trace | Prefer_predicted_core | Consider_only_predicted_core

(**
When a path of activation and inhibition arrows is found to explain an event,
this setting indicates which events of the path should be added to the core :
Do_not_minimize -> every event of the path
Minimize -> a subset of the events that is sufficient to guarantee that the core computed
will highlight the necessary activations between the inhibited event and the explained event
Do_not_impose_activation_path -> nothing is imposed to ensure that the compression algorithm
will highlight the necessary activations. Generates shorter but sometimes invalid experiments.
*)
type activation_paths_mode = Do_not_minimize | Minimize | Do_not_impose_activation_path
type configuration =
{
  (**
  The compression algorithm used. It must generate valid subtraces that contains the specified
  events of interest. No other assumption is made.
  *)
  compression_algorithm : Trace_explorer.t -> Causal_core.var_info_table -> int list -> int list;
  (** Give the cumulative core to the heuristic (for blocking events) instead of initial core. *)
  give_cumulative_core_to_heuristic : bool;
  (** Heuristic for blocking events. See the Heuristics module. *)
  heuristic    : Global_trace.t -> Ext_tools.IntSet.t -> int list -> int -> Resimulator_interface.interventions;
  (** Number of resimulations to sample after having determined the interventions. *)
  nb_samples   : int;
  (** Heuristic for choosing a trace among those that have been resimulated. *)
  trace_scoring_heuristic : Global_trace.t -> Global_trace.t -> int list -> int -> int ;
  (** Threshold for determining that an event is important.
  0.0 never consider an event as important.
  1.0 consider an event as important if at least one resimulation failed. *)
  threshold    : float;
  (** Max number of counterfactual experiments to compute. *)
  max_counterfactual_exps : int;
  (** Settings for searching counterfacutal->factual inhibitions. *)
  cf_inhibitions_finding_mode : inhibitions_finding_mode;
  cf_activation_paths_compression : activation_paths_mode;
  (** Settings for searching facutal->counterfactual inhibitions. *)
  fc_inhibitions_finding_mode : inhibitions_finding_mode;
  fc_activation_paths_compression : activation_paths_mode;
  (** Specify the max number of reasons to search for explaining an event. *)
  max_inhibitors_added_per_factual_events : int;
  max_inhibitors_added_per_cf_events : int;
  (** Determined whether or not every experiment must contains the initial story. *)
  include_initial_story_in_experiments : bool;
  (** Determines whether or not a common event must also be common in the compressed experiment.
  Must be set to true to generate valid experiments. *)
  add_common_events_to_both_cores : bool;
  (** Determines whether or not every event that is not common must be explained.
  Must be set to true to generate valid experiments, but it generates longer stories.
  Not needed until you really want a valid experiment. *)
  compute_inhibition_arrows_for_every_events : bool;
  (* Recompute some explanations even for events that have already been explained.
  Advised if Consider_only_predicted_core is used. *)
  adjust_inhibition_arrows_with_new_core_predictions : bool;
}

val compute_extended_story : Trace_explorer.t -> int -> configuration -> extended_story

val kaflow_compression : Trace_explorer.t -> Causal_core.var_info_table -> int list -> int list
