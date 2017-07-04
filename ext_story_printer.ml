open Story_printer
open Ext_tools

type print_options =
  { ranksep            : float  ;
    show_strong_deps   : bool   ;
    strong_deps_labels : bool   ;
    dump_grid          : bool   ;
    show_event_ids     : bool   ;
    font               : string ;
  }

type merging_mode = Hiding_factual_events | Merging_factual_events | No_merging

(* Different merging modes :
    - Show only counterfactual-only events of counterfactual parts (+annotations)
    - Show everything without merging (many nodes can correspond to the same factual event)
    - Merge common factual events BUT if they have different causes, show all these relations with different colors
*)

let compute_precedence model subtrace =
    let (grid, _) = Grid.build_grid model (trace_to_ttrace subtrace) in
    let core = n_first_intergers (Array.length grid) in
    let precedences = Precedence.transitive_reduction (Precedence.compute_precedence grid core) in
    List.map (fun (i1,i2) -> (index_to_id subtrace i1,index_to_id subtrace i2)) precedences

let compute_activation model subtrace =
    let (grid, _) = Grid.build_grid model (trace_to_ttrace subtrace) in
    let core = n_first_intergers (Array.length grid) in
    let activations = Precedence.compute_strong_deps model grid core in
    List.map (fun (i1,c,i2) -> (index_to_id subtrace i1,c,index_to_id subtrace i2)) activations

let print_factual_event already_printed fmt step = ()
let print_precedence fmt (id1,id2) = ()
let print_activation fmt (id1,c,id2) = ()

let print_factual_part model fact print_act fmt =
    let pr x = Format.fprintf fmt x in
    List.iter (print_factual_event [] fmt) fact ;
    let prec = compute_precedence model fact in
    List.iter (print_precedence fmt) prec ;
    pr "@;" ;
    if print_act then
    (
        let act = compute_activation model fact in
        List.iter (print_activation fmt) act ;
        pr "@;"
    ) ;
    List.map (fun ev -> get_id ev) fact

let print_extended_story model (fact,cps) mode options fmt =
    let pr x = Format.fprintf fmt x in

    pr "@[<v 2>digraph G{@;" ;
    pr "rankdir=\"TB\";@;" ;
    pr "ranksep=%.2f;@;" options.ranksep ;
    pr "node [fontname=\"%s\"];@;" options.font ;
    pr "edge [fontname=\"%s\"];@;" options.font ;
    pr "@;" ;

    let factual_already_printed = print_factual_part model fact options.show_strong_deps fmt in
    (* TODO *) ()

(*
let activations = Precedence.compute_strong_deps model cf_grid cf_core in
let activations = List.map (fun (i1,c,i2) -> (index_to_id cf_trace i1,c,index_to_id cf_trace i2)) activations in
let precedences = Precedence.transitive_reduction (Precedence.compute_precedence cf_grid cf_core) in
let precedences = List.map (fun (i1,i2) -> (index_to_id cf_trace i1,index_to_id cf_trace i2)) precedences in
*)
