open Interface

let heuristic_choose_interventions () : interventions = []

let main () =
  (* Compute factual causal core *)
  (* Choose intervention (heuristic) *)
  let interventions = heuristic_choose_interventions () in
  (* Compute and sample counterfactuals traces *)
  (* Find the most blocked event in the causal core *)
  ()

let () = main ()
