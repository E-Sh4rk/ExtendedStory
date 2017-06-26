open Interface
open Ext_tools

let heuristic_choose_interventions () : interventions = []

type configuration =
{
  nb_samples   : int;
  threshold    : float;
  more_inhibition_arrows : bool;
  more_relations_with_factual : bool;
  show_entire_counterfactual_stories : bool;
}

let compute_extended_story model trace rule_name config =
  (* Determining event of interest *)
  let eoi_id = 0 in
  (* Compute factual causal core *)
  (* Initialize a list of counterfactual extended causal cores *)
  
  (* Choose intervention (heuristic) depending on the trace and the current factual causal core :
  For example :
	- Block permanently in trace T every event that involve species in the factual core and that is not in the factual causal core.
	- Block permanently an event that is suspected to have an impact later.*)
  let interventions = heuristic_choose_interventions () in
  let block_pred = interventions_to_predicate interventions
  and stop_pred = stop_conditions_to_predicate [Event_has_happened eoi_id;Event_has_not_happened eoi_id] in
  (* Compute and sample counterfactuals traces (resimulation stops when eid has happened/has been blocked) *)
  let samples = Array.make config.nb_samples trace in
  let samples = Array.map (fun t -> resimulate model block_pred stop_pred) samples in
  (* If the threshold is excedeed : *)
  (* Take one of the counterfactual traces (heuristic? random among the traces that block the eoi? smallest core? more blocked event?) *)
  (* Find the last events that has inhibited the first event of the causal core that has been blocked :
  it is the last events that changed the value of a tested logical site from a good value to a wrong value. *)
  (* Select the first (earliest) of these events and compute its causal core. Add this counterfactual causal core to the list and indicate where go the inhibition arrow. *)
  (* For each events of this counterfactual causal core <that has no counterfactual-only cause|that as at least one factual cause>,
  find the last events in the factual trace among those that we blocked that prevent it (same method as above). Indicate in the counterfactual core the origin of these inhibition arrows. *)
  (* Update the factual core : compute a new factual causal core with all the previous added events + these inhibitive events <+ other factual events of the counterfactual core if we want to have more links with the factual core at the end>. *)
  
  (* At the end, merge the factual causal core and all the counterfactual causal cores. Depending on the details wanted by the user, we can :
     - Merge everything by merging together nodes that represent the same event
     - Keep only counterfactual-only events of counterfactual cores
     Don't forget to put all the inhibition arrows that had been found.
  *)
  ()
