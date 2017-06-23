open Interface
open Ext_tools

let heuristic_choose_interventions () : interventions = []

let compute_extended_story model trace rule_name =
  (* Compute factual causal core *)
  (* Initialize a list of counterfactual extended causal cores *)
  
  (* Choose intervention (heuristic) depending on the trace and the current extended story :
  For example :
	- Block permanently in trace T every event that involve species in the factual core and that is not in S.
	- Block permanently an event that is suspected to have an impact later.*)
  let interventions = heuristic_choose_interventions () in
  (* Compute and sample counterfactuals traces (resimulation stops when one event of the causal core is blocked) *)
  (* If the threshold is excedeed : *)
  (* Take one of the counterfactual traces (heuristic? random?) *)
  (* Find the last events that has inhibited the event of the causal core that has been blocked :
  it is the last events that changed the value of a tested logical site from a good value to a wrong value. *)
  (* Select the first (earliest) of these events and compute its causal core. Add this counterfactual causal core to the list and indicate where go the inhibition arrow. *)
  (* For each events of this counterfactual causal core <that has no counterfactual-only cause|that as at least one factual cause>,
  find the last events in the factual trace among those that we blocked that prevent it (same method as above). Indicate in the counterfactual core the origin of this inhibition arrow. *)
  (* Compute a new factual causal core with all the previous events + these inhibitive events <+ other factual events of the counterfactual core if we want to have more links with the factual core at the end>. *)
  
  (* At the end, merge the factual causal core and all the counterfactual causal cores. Depending on the details wanted by the user, we can :
     - Merge everything by merging together nodes that represent the same event
     - Keep only counterfactual-only events of counterfactual cores
	Don't forget to put inhibition arrows that had been found.
  *)
  ()
