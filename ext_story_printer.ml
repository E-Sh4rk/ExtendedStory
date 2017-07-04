open Story_printer

type merging_mode = Hiding_factual_events | Merging_factual_events | No_merging

(* Different merging modes :
    - Show only counterfactual-only events of counterfactual parts (+annotations)
    - Show everything without merging (many nodes can correspond to the same factual event)
    - Merge common factual events BUT if they have different causes, show all these relations with different colors
*)

let print_extended_story es =   (* TODO *) ()

(*
let activations = Precedence.compute_strong_deps model cf_grid cf_core in
let activations = List.map (fun (i1,c,i2) -> (index_to_id cf_trace i1,c,index_to_id cf_trace i2)) activations in
let precedences = Precedence.transitive_reduction (Precedence.compute_precedence cf_grid cf_core) in
let precedences = List.map (fun (i1,i2) -> (index_to_id cf_trace i1,index_to_id cf_trace i2)) precedences in
*)
