open Resimulator_interface
open Ext_tools
open Global_trace

type cf_part = Global_trace.t * ((int * Grid.constr * int) list) (* (subtrace, inhibition arrows) *)
type extended_story = Global_trace.t * (cf_part list) (* (subtrace, counterfactual parts) *)

type configuration =
{
  heuristic    : Global_trace.t -> int list -> int -> Resimulator_interface.interventions;
  nb_samples   : int;
  max_rejections   : int;
  threshold    : float;
  max_counterfactual_parts : int;
  precompute_cf_cores : bool;
  max_cf_inhibition_arrows : int;
  max_fc_inhibition_arrows_per_inhibator : int;
  add_all_factual_events_involved_to_factual_core : bool;
}

let compute_causal_core trace eois =
  let eois = List.sort_uniq Pervasives.compare eois in
  Causal_core.core_events (Causal_core.compute_causal_core (get_trace_explorer trace) (get_var_infos trace) eois)

let cf_trace_rejected eoi trace cf_trace =
  let model = get_model trace in
  let name = get_step_name model (get_step trace eoi) "" in
  let cf_index_eq = search_last_before_order cf_trace (get_order trace eoi) in
  let cf_index_eq = match cf_index_eq with None -> -1 | Some i -> i in
  let rec aux i = match i with
  | i when i < 0 -> false
  | i when get_step_name model (get_step cf_trace i) "" = name
  && get_global_id cf_trace i < 0 -> true
  | i -> aux (i-1)
  in aux cf_index_eq

let cf_trace_succeed eoi_id cf_trace =
  match search_global_id cf_trace eoi_id with
  | None -> false
  | Some _ -> true

let resimulate_and_sample trace nb max_rejections eoi block_pred stop_pred =
  let rec aux n nb_rej nb_failed wit = match n, nb_rej with
    | 0, _ -> (nb,nb_rej,nb_failed,wit)
    | n, nb_rej when nb_rej > max_rejections -> (nb-n,nb_rej,nb_failed,wit)
    | n, nb_rej ->
    let cf_trace = resimulate block_pred stop_pred trace in
    (*dbg (Format.asprintf "%a" Global_trace.print cf_trace) ;*)
    if cf_trace_rejected eoi trace cf_trace
    then aux n (nb_rej+1) nb_failed wit
    else if cf_trace_succeed (get_global_id trace eoi) cf_trace then
    aux (n-1) nb_rej nb_failed wit
    else aux (n-1) nb_rej (nb_failed+1) (Some cf_trace)
  in aux nb 0 0 None

let rec last_inhibitive_event_before trace index constr =
  let Grid.Constr (var, _) = constr in
  let history = get_history trace var in
  let last = History.last_before index history in
  match last with
  | None -> None
  | Some i ->
  if List.exists (fun c -> c=constr) (get_actions trace i)
  then None (* It is an activation *)
  else (
    if List.exists (fun c -> c=constr) (get_tests trace i)
    then Some i
    else last_inhibitive_event_before trace i constr
  )

let rec last_between_among history among index1 index2 =
  match History.last_before index2 history, among with
  | None, _ -> None
  | Some n, _ when n <= index1 -> None
  | Some n, None -> Some n
  | Some n, Some among when IntSet.mem n among -> Some n
  | Some n, _ -> last_between_among history among index1 n

let rec last_activation_event_between trace among index1 index2 constr =
  let Grid.Constr (var, _) = constr in
  let history = get_history trace var in
  let last = last_between_among history among index1 index2 in
  match last with
  | None -> None
  | Some i ->
  if List.exists (fun c -> c=constr) (get_actions trace i)
  then
  (
    if List.exists (fun c -> c=constr) (get_tests trace i)
    then last_activation_event_between trace among index1 i constr
    else Some i 
  ) 
  else None (* It is an inhibition *)

let activation_event_between trace follow_core index1 index2 constr =
  let i = last_activation_event_between trace None index1 index2 constr in
  if follow_core <> None && i <> None
  then
  (
    let i' = last_activation_event_between trace follow_core index1 index2 constr in
    if i' = None then i else i'
  )
  else i

type inhibition_reason =
  | Inhibition of int * Grid.constr * int
  | No_reason of int

let find_inhibitive_arrows trace1 trace2 follow_core eoi1 =
  let rec rewind (src,c,dest) =
    let index1_eq = search_first_after_order trace1 (get_order trace2 src) in
    let index1_eq = match index1_eq with None -> length trace1 | Some i -> i in
    let act = activation_event_between trace1 follow_core (index1_eq-1) dest c in
    match act with
    | None ->
    (*dbg (Format.asprintf "Inh : %d (%d) -> %d (%d)" src (get_global_id trace2 src) dest (get_global_id trace1 dest)) ;*)
    [Inhibition (src,c,dest)]
    | Some i -> aux i
  and aux dest =
    let index2_eq = search_last_before_order trace2 (get_order trace1 dest) in
    let index2_eq = match index2_eq with None -> -1 | Some i -> i in
    let inh = List.map (fun c -> (last_inhibitive_event_before trace2 (index2_eq+1) c, c)) (get_tests trace1 dest) in
    let inh = List.filter (fun (opt,_) -> opt <> None) inh in
    let inh = List.map (function (Some src,c) -> (src,c,dest) | _ -> assert false) inh in
    match inh with
    | [] -> [No_reason (dest)]
    | inh -> List.flatten (List.map rewind inh)
  in aux eoi1

let choose_arrows arrows nb =
  let cmp (s,_,d) (s',_,d') = match Pervasives.compare s s' with
  | 0 -> Pervasives.compare d d'
  | n -> n
  in
  let arrows = List.sort cmp arrows in
  cut_after_index (nb-1) arrows

let factual_events_of_trace trace =
  let rec aux acc i = match i with
  | i when i < 0 -> acc
  | i when get_global_id trace i >= 0 -> aux (IntSet.add i acc) (i-1)
  | i -> aux acc (i-1) in
  aux (IntSet.empty) ((length trace)-1)

 let add_cf_parts trace eoi core config =
   let rec aux core cf_parts events_in_factual =
    (* Choose intervention (heuristic) depending on the trace and the current factual causal core. *)
    logs "Determining interventions (heuristic)..." ;
    let interventions = config.heuristic trace core eoi in
    (*dbg (Format.asprintf "%a" Resimulator_interface.print interventions) ;*)
    let scs = [Event_has_happened eoi;Event_has_not_happened eoi] in
    (* Compute and sample counterfactual traces (resimulation stops when eoi has happened/has been blocked) *)
    (* Take one of the counterfactual traces that failed as witness (heuristic? random among the traces that block the eoi? smallest core?) *)
    logs (Format.asprintf "%a. Resimulating..." Resimulator_interface.print_short interventions) ;
    let (nb_samples,nb_rej,nb_failed,cf_trace) = resimulate_and_sample trace config.nb_samples config.max_rejections eoi interventions scs in
    let ratio = if nb_samples > 0 then 1.0 -. (float_of_int nb_failed)/.(float_of_int nb_samples) else 1.0 in
    logs ((string_of_int nb_rej)^" rejected. Ratio : "^(string_of_float ratio)) ;
    if ratio >= config.threshold || List.length cf_parts >= config.max_counterfactual_parts then (core, cf_parts)
    else
    (
      logs "Computing counterfactual experiment..." ;
      let cf_trace = match cf_trace with Some ctrace -> ctrace | None -> assert false in
      (* Find the inhibitors of eoi in the cf trace, and eventually filter them. *)
      let reasons = find_inhibitive_arrows trace cf_trace (Some (IntSet.of_list core)) eoi in
      let inhibitors_cf = List.map (function Inhibition (s,c,d) -> (s,c,d) | No_reason _ -> assert false)
      (List.filter (function No_reason _ -> false | Inhibition _ -> true) reasons) in
      let (cf_part, events_in_factual) = if inhibitors_cf = [] then
      begin
        (* If there is no inhibitors (bad heuristic, direct cause blocked), we add direct cause blocked to the core in order to avoid infinite loop *)
        logs "Bad heuristic (no inhibitor) ! Adding direct causes to causal core in order to avoid infinite loop..." ;
        let direct_causes = List.map (function No_reason i -> i | Inhibition _ -> assert false)
        (List.filter (function No_reason _ -> true | Inhibition _ -> false) reasons) in
        (None, IntSet.union events_in_factual (IntSet.of_list direct_causes))
      end
      else begin
        let inhibitors_cf = choose_arrows inhibitors_cf config.max_cf_inhibition_arrows in
        let inhibitors_cf_ids = List.map (fun (src,c,dest) -> get_global_id cf_trace src, c, get_global_id trace dest) inhibitors_cf in
        (* Find the inhibitors of cf_eois in the factual trace, and eventually filter them *)
        let cf_eois = List.fold_left (fun acc (s,_,_) -> IntSet.add s acc) IntSet.empty inhibitors_cf in
        let cf_pre_core = if config.precompute_cf_cores
        then Some (IntSet.of_list (compute_causal_core cf_trace (IntSet.elements cf_eois)))
        else None in
        let reasons = List.map (fun e -> find_inhibitive_arrows cf_trace trace cf_pre_core e) (IntSet.elements cf_eois) in
        let inhibitors_fc = List.map (fun reasons -> List.map (function Inhibition (s,c,d) -> (s,c,d) | No_reason _ -> assert false)
        (List.filter (function No_reason _ -> false | Inhibition _ -> true) reasons)) reasons in
        let inhibitors_fc = List.map (fun inh -> choose_arrows inh config.max_fc_inhibition_arrows_per_inhibator) inhibitors_fc in
        let inhibitors_fc = List.flatten inhibitors_fc in
        let inhibitors_fc_ids = List.map (fun (src,c,dest) -> get_global_id trace src, c, get_global_id cf_trace dest) inhibitors_fc in
        (* Now we have all inhibition arrows *)
        let inhibitions_ids = inhibitors_fc_ids@inhibitors_cf_ids in
        let cf_involved = IntSet.union cf_eois (List.fold_left (fun acc (_,_,d) -> IntSet.add d acc) IntSet.empty inhibitors_fc) in
        let f_involved = IntSet.union (List.fold_left (fun acc (_,_,d) -> IntSet.add d acc) IntSet.empty inhibitors_cf)
        (List.fold_left (fun acc (s,_,_) -> IntSet.add s acc) IntSet.empty inhibitors_fc) in
        (* Compute the causal core associated with the cf events involved *)
        logs ((string_of_int (List.length inhibitions_ids))^" inhibition arrows found ! Computing new causal cores...") ;
        let cf_core = compute_causal_core cf_trace (IntSet.elements cf_involved) in
        let cf_subtrace = subtrace_of cf_trace cf_core in
        (* Retrieving events to add to the factual core : factual events with an inhibition arrow
        + other factual events of the counterfactual core if we want to have more links with the factual core *)
        let events_in_factual = IntSet.union f_involved events_in_factual in
        let events_in_factual = if config.add_all_factual_events_involved_to_factual_core
        then IntSet.union (factual_events_of_trace cf_subtrace) events_in_factual
        else events_in_factual in
        (Some (cf_subtrace,inhibitions_ids), events_in_factual)        
      end
      in
      (* Update the factual core *)
      let core = compute_causal_core trace (IntSet.elements events_in_factual) in
      let cf_parts = match cf_part with None -> cf_parts | Some cfp -> cfp::cf_parts in
      aux core cf_parts events_in_factual
    )
   in aux core [] (IntSet.singleton eoi)

let compute_extended_story trace eoi config : extended_story =
  let trace = new_reference_trace trace in
  (*dbg (Format.asprintf "Trace length : %d" (length trace)) ;*)
  (*dbg (Format.asprintf "EOI : %d" eoi) ;*)
  (* Computing factual causal core *)
  logs "Computing initial factual core..." ;
  let core = compute_causal_core trace [eoi] in
  (*dbg (Format.asprintf "Core : %a" (print_core trace) core) ;*)
  (* Adding counterfactual parts *)
  let (core, cf_parts) = add_cf_parts trace eoi core config in
  let subtrace = subtrace_of trace core in
  logs "Extended story complete !" ; (subtrace, cf_parts)
