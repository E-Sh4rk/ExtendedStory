open Interface
open Ext_tools

let heuristic_choose_interventions () : interventions = []

let compute_extended_story model trace rule_name =
  (* Compute factual causal core *)
  (* Convert a copy of this causal core to an extended story *)
  
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
  (* Select the first of these events and compute its causal core *)
  (* Merge this contrefactual core with the extended story, add the corresponding inhibition arrow, and add missing precendence relations between them *)
  (* For each events of the contrefactual core that is caused directly by an event present in the factual core (including init) :
  Find the last events in the factual trace among those that we blocked that prevent it (same method as above)
  (it should have happenned between the factual and contrefactual events), compute their causal cores,
  merge them and add corresponding inhibition arrows. *)
  ()
