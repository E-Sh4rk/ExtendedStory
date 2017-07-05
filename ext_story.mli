open Ext_tools

type cf_part = (step list) * ((int * Grid.constr * int) list) (* (subtrace, inhibition arrows) *)
type extended_story = (step list) * (cf_part list) (* (subtrace, counterfactual parts) *)

type configuration =
{
  nb_samples   : int;
  threshold    : float;
  max_counterfactual_parts : int;
  precompute_cf_cores : bool;
  max_cf_inhibition_arrows : int;
  max_fc_inhibition_arrows_per_inhibator : int;
  add_all_factual_events_involved_to_factual_core : bool;
}

val compute_extended_story : Model.t -> Trace.t -> string -> configuration -> extended_story
