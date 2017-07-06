open Ext_tools

type merging_mode = Hiding_factual_events | Merging_factual_events | No_merging

(* Different merging modes :
    - Show only counterfactual-only events of counterfactual parts (+annotations)
    - Show everything without merging (many nodes can correspond to the same factual event)
    - Merge common factual events BUT if they have different causes, show all these relations with different colors
*)

let max_time te = get_time_of_step (Trace_explorer.step (Trace_explorer.last_step_id te) te) 0.0

let choose_color options tr i dummy =
    match options.Story_printer.nodes_coloring with
    | Build_order -> failwith "Build order coloring not implemented."
    | Time ->
        let maxT = max_time (Global_trace.get_trace_explorer tr) in
        let t = get_time_of_step (Global_trace.get_step tr i) 0.0 in
        let (r,v,b) = (0., 0., (1.0 -. 0.4 *. t /. maxT)) in
        if Global_trace.get_global_id tr i < 0 then (b,v,r) else (r,v,b)

let compute_precedence subtrace =
    let core = n_first_intergers (Global_trace.length subtrace) in
    let precedences = Precedence.transitive_reduction (Precedence.compute_precedence (Global_trace.get_trace_explorer subtrace) core) in
    List.map (fun (i1,i2) -> (Global_trace.get_global_id subtrace i1,Global_trace.get_global_id subtrace i2)) precedences

let compute_activation subtrace =
    let core = n_first_intergers (Global_trace.length subtrace) in
    let activations = Precedence.compute_strong_deps (Global_trace.get_trace_explorer subtrace) core in
    List.map (fun (i1,c,i2) -> (Global_trace.get_global_id subtrace i1,c,Global_trace.get_global_id subtrace i2)) activations

let print_factual_event trace index already_printed options fmt =
    let id = Global_trace.get_global_id trace index in
    if not (IntSet.mem id already_printed) then
    Story_printer.print_event options (Global_trace.get_trace_explorer trace) (choose_color options trace) fmt id (index,index)

let print_precedence fmt (id1,id2) = ()
let print_activation fmt (id1,c,id2) = ()

let print_factual_part fact options fmt =
    let pr x = Format.fprintf fmt x in
    for i=0 to (Global_trace.length fact)-1 do
        print_factual_event fact i IntSet.empty options fmt
    done ;
    let prec = compute_precedence fact in
    List.iter (print_precedence fmt) prec ;
    pr "@;" ;
    if options.Story_printer.show_strong_deps then
    (
        let act = compute_activation fact in
        List.iter (print_activation fmt) act ;
        pr "@;"
    ) ;
    let core = n_first_intergers (Global_trace.length fact) in
    List.map (fun i -> Global_trace.get_global_id fact i) core

let print_extended_story (fact,cps) mode options fmt =
    let pr x = Format.fprintf fmt x in

    pr "@[<v 2>digraph G{@;" ;
    pr "rankdir=\"TB\";@;" ;
    pr "ranksep=%.2f;@;" options.Story_printer.ranksep ;
    pr "node [fontname=\"%s\"];@;" options.Story_printer.font ;
    pr "edge [fontname=\"%s\"];@;" options.Story_printer.font ;
    pr "@;" ;

    let factual_already_printed = print_factual_part fact options fmt in
    (* TODO *) ()
