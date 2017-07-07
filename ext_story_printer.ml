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
        let (h,s,v) = (0., 0., (1.0 -. 0.4 *. t /. maxT)) in
        if Global_trace.get_global_id tr i < 0 then (0.5,0.5,v) else (h,s,v)

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
    if (not (IntSet.mem id already_printed) || mode = No_merging) && (mode <> Hiding_factual_events || part_nb = 0) then
    (
        let gid = id_to_gid part_nb id in
        Story_printer.print_event options (Global_trace.get_trace_explorer trace) (choose_color options trace) fmt gid (index,index) ;
        IntSet.add id already_printed
    ) else already_printed

let print_cf_event trace part_nb index id_to_gid (mode,options) fmt =
    let id = Global_trace.get_global_id trace index in
    let gid = id_to_gid part_nb id in
    Story_printer.print_event options (Global_trace.get_trace_explorer trace) (choose_color options trace) fmt gid (index,index)

let print_event trace part_nb index fap id_to_gid (mode,options) fmt =
    let id = Global_trace.get_global_id trace index in
    if id >= 0 then print_factual_event trace part_nb index fap id_to_gid (mode,options) fmt
    else (print_cf_event trace part_nb index id_to_gid (mode,options) fmt ; fap)

let print_precedence part_nb id_to_gid options fmt (id1,id2) =
    let gid1 = id_to_gid part_nb id1 and gid2 = id_to_gid part_nb id2 in
    Story_printer.print_prec_arrow options fmt (gid1,gid2)

let print_activation part_nb id_to_gid env options fmt (id1,c,id2) =
    let gid1 = id_to_gid part_nb id1 and gid2 = id_to_gid part_nb id2 in
    Story_printer.print_strong_dep_arrow options env fmt (gid1,c,gid2)

let print_factual_part fact id_to_gid (mode,options) fmt =
    let pr x = Format.fprintf fmt x in

    let core = n_first_intergers (Global_trace.length fact) in
    let fap = List.fold_left (fun acc i -> print_factual_event fact 0 i acc id_to_gid (mode,options) fmt) IntSet.empty core in
    let prec = compute_precedence fact in
    List.iter (print_precedence 0 id_to_gid options fmt) prec ;
    pr "@;" ;
    if options.Story_printer.show_strong_deps then
    (
        let act = compute_activation fact in
        List.iter (print_activation 0 id_to_gid (Global_trace.get_model fact) options fmt) act ;
        pr "@;"
    ) ;
    fap

let print_inh_arrow options env fmt (src, constr, dest) =
    let open Grid in
      Format.fprintf fmt "%d -> %d [%sfontsize=9, arrowhead=\"inv\"] @[<h>// %a@]@;" src dest
        (if options.Story_printer.strong_deps_labels then
           let (Constr (x, _v)) = constr in
           Format.asprintf "label=\"%a\", "
             (print_var env) (Var x)
         else "")
        (print_constr env "=") constr

let print_inhibition part_nb id_to_gid env options fmt (id1,c,id2) =
    let gid1 = id_to_gid part_nb id1 and gid2 = id_to_gid part_nb id2 in
    print_inh_arrow options env fmt (gid1,c,gid2)

let print_counterfactual_part (tr,inh) part_nb fap id_to_gid (mode,options) fmt =
    let pr x = Format.fprintf fmt x in

    let core = n_first_intergers (Global_trace.length tr) in
    let fap = List.fold_left (fun acc i -> print_event tr 0 i acc id_to_gid (mode,options) fmt) IntSet.empty core in
    pr "@;" ;
    List.iter (print_inhibition part_nb id_to_gid (Global_trace.get_model tr) options fmt) inh ;
    pr "@;" ;

    let prec = compute_precedence tr in
    let prec = if mode = Hiding_factual_events
    then List.filter (fun (s,d) -> s < 0 && d < 0) prec
    else prec in
    List.iter (print_precedence 0 id_to_gid options fmt) prec ;
    pr "@;" ;
    if options.Story_printer.show_strong_deps then
    (
        let act = compute_activation tr in
        let act = if mode = Hiding_factual_events
        then List.filter (fun (s,_,d) -> s < 0 && d < 0) act
        else act in
        List.iter (print_activation 0 id_to_gid (Global_trace.get_model tr) options fmt) act ;
        pr "@;"
    ) ;
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
    pr "@;}@]@." ; logs "Printing finished."
