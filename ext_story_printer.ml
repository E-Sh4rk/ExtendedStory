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
    let core = n_first_intergers (Global_trace.length subtrace) in
    let precedences = Precedence.transitive_reduction (Precedence.compute_precedence (Global_trace.get_trace_explorer subtrace) core) in
    List.map (fun (i1,i2) -> (Global_trace.get_global_id subtrace i1,Global_trace.get_global_id subtrace i2)) precedences

let compute_activation model subtrace =
    let core = n_first_intergers (Global_trace.length subtrace) in
    let activations = Precedence.compute_strong_deps (Global_trace.get_trace_explorer subtrace) core in
    List.map (fun (i1,c,i2) -> (Global_trace.get_global_id subtrace i1,c,Global_trace.get_global_id subtrace i2)) activations

let print_factual_event already_printed fmt id step = ()
let print_precedence fmt (id1,id2) = ()
let print_activation fmt (id1,c,id2) = ()

let print_factual_part model fact print_act fmt =
    let pr x = Format.fprintf fmt x in
    for i=0 to (Global_trace.length fact)-1 do
        print_factual_event [] fmt (Global_trace.get_global_id fact i) (Global_trace.get_step fact i)
    done ;
    let prec = compute_precedence model fact in
    List.iter (print_precedence fmt) prec ;
    pr "@;" ;
    if print_act then
    (
        let act = compute_activation model fact in
        List.iter (print_activation fmt) act ;
        pr "@;"
    ) ;
    let core = n_first_intergers (Global_trace.length fact) in
    List.map (fun i -> Global_trace.get_global_id fact i) core

let print_extended_story (fact,cps) mode options fmt =
    let pr x = Format.fprintf fmt x in
    let model = Global_trace.get_model fact in

    pr "@[<v 2>digraph G{@;" ;
    pr "rankdir=\"TB\";@;" ;
    pr "ranksep=%.2f;@;" options.ranksep ;
    pr "node [fontname=\"%s\"];@;" options.font ;
    pr "edge [fontname=\"%s\"];@;" options.font ;
    pr "@;" ;

    let factual_already_printed = print_factual_part model fact options.show_strong_deps fmt in
    (* TODO *) ()
