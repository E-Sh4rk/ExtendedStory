open Interface

let heuristic_choose_interventions () : interventions = []

let main () =
  (* Compute factual causal core *)
  (* Choose intervention (heuristic) *)
  let interventions = heuristic_choose_interventions () in
  (* Compute and sample counterfactuals traces *)
  (* If the htreshol is excedeed : *)
  (* Find the most blocked event in the causal core *)
  (* Find the last event that inhibits it in each counterfactual trace *)
  (* Select one of them (heuristic?) and compute its causal core *)
  (* Merge the two causal cores *)
  (* For each events of the contrefactual story that is caused directly by an event present in the factual story (including init) :
  Find the last event in the factual trace (among those that we blocked) that prevent it
  (it should have happenned between the factual and contrefactual events), compute its story and merge.
  Put an inhibition arrow between these two events. *)
  ()

let () = main ()
