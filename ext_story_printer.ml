open Ext_tools

type merging_mode = Hiding_factual_events | Merging_factual_events | No_merging

(* Different merging modes :
    - Show only counterfactual-only events of counterfactual parts (+annotations)
    - Show everything without merging (many nodes can correspond to the same factual event)
    - Merge common factual events BUT if they have different causes, show all these relations with different colors
*)

let max_time te = get_time_of_step (Trace_explorer.step (Trace_explorer.last_step_id te) te) 0.0

let id_to_gid mode fact part_nb id =
    if id < 0 || mode <> No_merging then id else
    (Global_trace.length fact)*part_nb + id

let choose_color options tr i _ =
    match options.Story_printer.nodes_coloring with
    | Story_printer.Build_order -> failwith "Build order coloring not implemented."
    | Story_printer.Time ->
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

let print_factual_event trace part_nb index already_printed id_to_gid (mode,options) fmt =
    let id = Global_trace.get_global_id trace index in
    if not (IntSet.mem id already_printed) || mode = No_merging then
    (
        let gid = id_to_gid part_nb id in
        Story_printer.print_event options (Global_trace.get_trace_explorer trace) (choose_color options trace) fmt gid (index,index)
    )

let print_precedence part_nb id_to_gid options fmt (id1,id2) =
    let gid1 = id_to_gid part_nb id1 and gid2 = id_to_gid part_nb id2 in
    Story_printer.print_prec_arrow options fmt (gid1,gid2)

let print_activation part_nb id_to_gid env options fmt (id1,c,id2) =
    let gid1 = id_to_gid part_nb id1 and gid2 = id_to_gid part_nb id2 in
    Story_printer.print_strong_dep_arrow options env fmt (gid1,c,gid2)

let print_factual_part fact id_to_gid (mode,options) fmt =
    let pr x = Format.fprintf fmt x in
    for i=0 to (Global_trace.length fact)-1 do
        print_factual_event fact 0 i IntSet.empty id_to_gid (mode,options) fmt
    done ;
    let prec = compute_precedence fact in
    List.iter (print_precedence 0 id_to_gid options fmt) prec ;
    pr "@;" ;
    if options.Story_printer.show_strong_deps then
    (
        let act = compute_activation fact in
        List.iter (print_activation 0 id_to_gid (Global_trace.get_model fact) options fmt) act ;
        pr "@;"
    ) ;
    let core = n_first_intergers (Global_trace.length fact) in
    List.map (fun i -> Global_trace.get_global_id fact i) core

let print_counterfactual_part cp part_nb fap id_to_gid (mode,options) fmt =

    fap

let print_extended_story (fact,cps) mode options fmt =
    let pr x = Format.fprintf fmt x in
    let id_to_gid = id_to_gid mode fact in

    pr "@[<v 2>digraph G{@;" ;
    pr "rankdir=\"TB\";@;" ;
    pr "ranksep=%.2f;@;" options.Story_printer.ranksep ;
    pr "node [fontname=\"%s\"];@;" options.Story_printer.font ;
    pr "edge [fontname=\"%s\"];@;" options.Story_printer.font ;
    pr "@;" ;

    let factual_already_printed = print_factual_part fact id_to_gid (mode,options) fmt in
    pr "@;" ;
    let _ = List.fold_left (fun (part_nb,fap) cp -> (part_nb+1,print_counterfactual_part cp part_nb fap id_to_gid (mode,options) fmt))
    (1,factual_already_printed) cps in
    pr "@;" ; logs "Printing finished."
