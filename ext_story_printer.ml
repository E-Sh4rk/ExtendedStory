open Ext_tools

type merging_mode = Hiding_factual_events | Merging_factual_events | No_merging

(* Different merging modes :
    - Show only counterfactual-only events of counterfactual parts (+annotations)
    - Show everything without merging (many nodes can correspond to the same factual event)
    - Merge common factual events BUT if they have different causes, show all these relations with different colors
*)

type general_settings =
{
    fmt : Format.formatter ;
    options : Story_printer.print_options ;
    mode : merging_mode ;
    model : Model.t ;
    nb_cf_parts : int ;
    factual_part : Global_trace.t ;
}

let id_to_gid settings part_nb id =
    if id < 0 || settings.mode <> No_merging then id else
    (Global_trace.length settings.factual_part)*part_nb + id

let max_time tr = get_time_of_step (Global_trace.get_step tr ((Global_trace.length tr)-1)) 0.0

let choose_node_color settings tr part_nb i _ =
    match settings.options.Story_printer.nodes_coloring with
    | Story_printer.Build_order -> failwith "Build order coloring not implemented."
    | Story_printer.Time ->
        let maxT = max_time tr in
        let t = get_time_of_step (Global_trace.get_step tr i) 0.0 in
        let (h,s,v) = (0., 0., (1.0 -. 0.4 *. t /. maxT)) in
        if Global_trace.get_global_id tr i < 0 then ((float_of_int part_nb)/.(float_of_int (settings.nb_cf_parts+1)),0.5,v) else (h,s,v)

let choose_edge_color settings part_nb =
    if part_nb = 0 then (0.,0.,0.) else
    ((float_of_int part_nb)/.(float_of_int (settings.nb_cf_parts+1)),0.5,0.5)

let compute_precedence subtrace =
    let core = n_first_intergers (Global_trace.length subtrace) in
    let precedences = Precedence.transitive_reduction (Precedence.compute_precedence (Global_trace.get_trace_explorer subtrace) core) in
    List.map (fun (i1,i2) -> (Global_trace.get_global_id subtrace i1,Global_trace.get_global_id subtrace i2)) precedences

let compute_activation subtrace =
    let core = n_first_intergers (Global_trace.length subtrace) in
    let activations = Precedence.compute_strong_deps ~compute_all_activations:true (Global_trace.get_trace_explorer subtrace) core in
    List.map (fun (i1,c,i2) -> (Global_trace.get_global_id subtrace i1,c,Global_trace.get_global_id subtrace i2)) activations

let print_factual_event settings trace part_nb index fap =
    let id = Global_trace.get_global_id trace index in
    if (not (IntSet.mem id fap) || settings.mode <> Merging_factual_events) && (settings.mode <> Hiding_factual_events || part_nb = 0) then
    (
        let gid = id_to_gid settings part_nb id in
        Story_printer.print_event settings.options (Global_trace.get_trace_explorer trace) (choose_node_color settings trace part_nb)
        settings.fmt gid (string_of_int id) (index,index) ;
        IntSet.add id fap
    ) else fap

let print_cf_event settings trace part_nb index =
    let id = Global_trace.get_global_id trace index in
    let gid = id_to_gid settings part_nb id in
    Story_printer.print_event settings.options (Global_trace.get_trace_explorer trace) (choose_node_color settings trace part_nb)
    settings.fmt gid (string_of_int id) (index,index)

let print_event settings trace part_nb index fap =
    let id = Global_trace.get_global_id trace index in
    if id >= 0 then print_factual_event settings trace part_nb index fap
    else (print_cf_event settings trace part_nb index ; fap)

let print_precedence settings part_nb (id1,id2) =
    let gid1 = id_to_gid settings part_nb id1 and gid2 = id_to_gid settings part_nb id2 in
    Story_printer.print_prec_arrow settings.options (choose_edge_color settings part_nb) settings.fmt (gid1,gid2)

let print_activation settings part_nb (id1,c,id2) =
    let gid1 = id_to_gid settings part_nb id1 and gid2 = id_to_gid settings part_nb id2 in
    Story_printer.print_strong_dep_arrow settings.options (choose_edge_color settings part_nb) settings.model settings.fmt (gid1,c,gid2)

let print_factual_part settings =
    let pr x = Format.fprintf settings.fmt x in

    let core = n_first_intergers (Global_trace.length settings.factual_part) in
    let fap = List.fold_left (fun acc i -> print_factual_event settings settings.factual_part 0 i acc) IntSet.empty core in
    let prec = compute_precedence settings.factual_part in
    List.iter (print_precedence settings 0) prec ;
    pr "@;" ;
    if settings.options.Story_printer.show_strong_deps then
    (
        let act = compute_activation settings.factual_part in
        List.iter (print_activation settings 0) act ;
        pr "@;"
    ) ;
    fap

let print_inh_arrow options color env fmt (src, constr, dest) =
    let open Grid in
      Format.fprintf fmt "%d -> %d [%sfontsize=9, arrowhead=\"tee\", color=%a] @[<h>// %a@]@;" src dest
        (if options.Story_printer.strong_deps_labels then
           let (Constr (x, _v)) = constr in
           Format.asprintf "label=\"%a\", "
             (print_var env) (Var x)
         else "")
        Story_printer.print_hsv_color color
        (print_constr env "=") constr

let print_inhibition settings part_nb (id1,c,id2) =
    let gid1 = id_to_gid settings part_nb id1 and gid2 = id_to_gid settings part_nb id2 in
    print_inh_arrow settings.options (choose_edge_color settings part_nb) settings.model settings.fmt (gid1,c,gid2)

let print_counterfactual_part settings (tr,inh) part_nb fap =
    let pr x = Format.fprintf settings.fmt x in

    let core = n_first_intergers (Global_trace.length tr) in
    let fap = List.fold_left (fun acc i -> print_event settings tr part_nb i acc) IntSet.empty core in
    pr "@;" ;
    List.iter (print_inhibition settings part_nb) inh ;
    pr "@;" ;

    let prec = compute_precedence tr in
    let prec = if settings.mode = Hiding_factual_events
    then List.filter (fun (s,d) -> (s < 0 || IntSet.mem s fap) && (d < 0 || IntSet.mem d fap)) prec
    else prec in
    List.iter (print_precedence settings part_nb) prec ;
    pr "@;" ;
    if settings.options.Story_printer.show_strong_deps then
    (
        let act = compute_activation tr in
        let act = if settings.mode = Hiding_factual_events
        then List.filter (fun (s,_,d) -> (s < 0 || IntSet.mem s fap) && (d < 0 || IntSet.mem d fap)) act
        else act in
        List.iter (print_activation settings part_nb) act ;
        pr "@;"
    ) ;
    fap

let print_extended_story (fact,cps) mode options fmt =
    let pr x = Format.fprintf fmt x in

    pr "@[<v 2>digraph G{@;" ;
    pr "rankdir=\"TB\";@;" ;
    pr "ranksep=%.2f;@;" options.Story_printer.ranksep ;
    pr "node [fontname=\"%s\"];@;" options.Story_printer.font ;
    pr "edge [fontname=\"%s\"];@;" options.Story_printer.font ;
    pr "@;" ;

    let settings =
    {
        fmt = fmt ;
        options = options ;
        mode = mode ;
        model = Global_trace.get_model fact ;
        nb_cf_parts = List.length cps ;
        factual_part = fact ;
    } in
    let fap = print_factual_part settings in
    pr "@;" ;
    let _ = List.fold_left (fun (part_nb,fap) cp -> (part_nb+1,print_counterfactual_part settings cp part_nb fap))
    (1,fap) cps in pr "@;}@]@."
